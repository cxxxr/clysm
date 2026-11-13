;;;; graphics.lisp — drawing (after the graphics ports of CAPI and the
;;;; drawing functions of CLIM)
;;;;
;;;; An OUTPUT-PANE shows what its DISPLAY-CALLBACK draws:
;;;;
;;;;   (make-instance 'output-pane
;;;;     :view-width 200 :view-height 100
;;;;     :display-callback
;;;;     (lambda (pane)
;;;;       (draw-rectangle pane 0 0 200 100 :foreground "navy")
;;;;       (dolist (city *cities*)
;;;;         (with-output-as-presentation (city 'city)
;;;;           (draw-circle pane (city-x city) (city-y city) 4 :filled t)))))
;;;;
;;;; The callback is called again when state it read has changed, and only
;;;; the shapes that are different change on the screen: a drawing is a tree
;;;; of shapes (SVG), not pixels.  So a shape can be a presentation — the
;;;; value of a drawing function is its node — and it scales with the pane.
;;;;
;;;; Compile it after pane.lisp.

(in-package "UI")

(defmodel output-pane (pane callbacks)
  (;; a function of the pane that draws with the functions below
   (display-callback :initarg :display-callback :initform nil :accessor output-pane-display-callback)
   ;; the size of the drawing in its own coordinates; it is scaled to the
   ;; pane.  Without it, the coordinates are the pixels of the pane.
   (view-width :initarg :view-width :initform nil :accessor output-pane-view-width)
   (view-height :initarg :view-height :initform nil :accessor output-pane-view-height)
   ;; functions of the pane and the position X, Y in the drawing where the
   ;; pointer was pressed, moved and released
   (press-callback :initarg :press-callback :initform nil :accessor output-pane-press-callback)
   (motion-callback :initarg :motion-callback :initform nil :accessor output-pane-motion-callback)
   (release-callback :initarg :release-callback :initform nil :accessor output-pane-release-callback))
  (:default-initargs :stretch t)
  (:documentation "A pane for drawing."))

(defvar *shapes* nil "The shapes drawn so far, the last first (bound while a pane is drawn).")
(defvar *drawing* nil "True while a pane is drawn.")
(defvar *graphics-state* nil "The plist of the graphics state in WITH-GRAPHICS-STATE.")

(defun pointer-listener (pane accessor)
  (lambda (&optional event)
    (let ((function (funcall accessor pane)))
      (when function
        (multiple-value-bind (x y) (event-position *backend* event)
          (funcall function pane x y))))))

(defmethod render-pane ((pane output-pane))
  (let ((shapes (let ((*shapes* '()) (*drawing* t) (*graphics-state* nil))
                  (when (output-pane-display-callback pane)
                    (funcall (output-pane-display-callback pane) pane))
                  (reverse *shapes*)))
        (width (output-pane-view-width pane))
        (height (output-pane-view-height pane)))
    ;; the listeners are there only if there are callbacks
    (element "svg"
             (list :class "ui-output-pane"
                   :viewbox (and width height (format nil "0 0 ~A ~A" (text-of width) (text-of height)))
                   :on-pointerdown (and (output-pane-press-callback pane)
                                        (pointer-listener pane #'output-pane-press-callback))
                   :on-pointermove (and (output-pane-motion-callback pane)
                                        (pointer-listener pane #'output-pane-motion-callback))
                   :on-pointerup (and (output-pane-release-callback pane)
                                      (pointer-listener pane #'output-pane-release-callback)))
             shapes)))

;;; ------------------------------------------------------------------
;;; The graphics state
;;;
;;; The drawing functions take these keyword arguments, whose defaults are
;;; those of the enclosing WITH-GRAPHICS-STATE:
;;;   :foreground   the colour (a CSS colour; by default that of the text)
;;;   :filled       true to fill the shape instead of drawing its outline
;;;   :fill         the colour inside an outlined shape
;;;   :thickness    the width of lines
;;;   :dashes       a list of the lengths of dashes and gaps
;;;   :opacity      from 0 to 1
;;;   :class        CSS classes of the shape

(defmacro with-graphics-state ((pane &rest state) &body body)
  "Draw what BODY draws with STATE (:FOREGROUND, :THICKNESS...) as the
defaults of the drawing functions."
  `(let ((*graphics-state* (list* ,@state *graphics-state*)))
     ,pane
     ,@body))

(defun number-text (x)
  (text-of (if (and (rationalp x) (not (integerp x))) (float x 1.0d0) x)))

(defun shape (pane tag properties options &rest children)
  "Add the shape TAG with PROPERTIES and the graphics state OPTIONS to the
drawing; return its node."
  (declare (ignore pane))
  (unless *drawing*
    (error "UI: a drawing function was called outside the DISPLAY-CALLBACK of an OUTPUT-PANE"))
  (flet ((option (key &optional default)
           (let ((value (getf options key '%none)))
             (if (eq value '%none) (getf *graphics-state* key default) value))))
    (let* ((foreground (option :foreground "currentColor"))
           (filled (option :filled))
           (dashes (option :dashes))
           (node (apply #'element tag
                        (append properties
                                (if (string= tag "text")
                                    (list :fill foreground)
                                    (list :fill (if filled foreground (option :fill "none"))
                                          :stroke (if filled (option :outline) foreground)
                                          :stroke-width (and (option :thickness)
                                                             (number-text (option :thickness)))
                                          :stroke-dasharray (and dashes
                                                                 (format nil "~{~A~^ ~}"
                                                                         (mapcar #'number-text dashes)))))
                                (list :opacity (and (option :opacity) (number-text (option :opacity)))
                                      :class (option :class)))
                        children)))
      (push node *shapes*)
      node)))

(defun draw-line (pane x1 y1 x2 y2 &rest options)
  (shape pane "line" (list :x1 (number-text x1) :y1 (number-text y1)
                           :x2 (number-text x2) :y2 (number-text y2))
         options))

(defun points-text (points)
  "POINTS is a list of x y x y... or of (x y)... or of (x . y)..."
  (let ((flat (if (consp (car points))
                  (loop for point in points
                        collect (car point)
                        collect (if (consp (cdr point)) (cadr point) (cdr point)))
                  points)))
    (format nil "~{~A,~A~^ ~}" (mapcar #'number-text flat))))

(defun draw-lines (pane points &rest options)
  "Draw the line through POINTS (x y x y..., or a list of (x y))."
  (shape pane "polyline" (list :points (points-text points)) options))

(defun draw-polygon (pane points &rest options)
  "Draw the closed line through POINTS (x y x y..., or a list of (x y))."
  (shape pane "polygon" (list :points (points-text points)) options))

(defun draw-rectangle (pane x y width height &rest options &key radius &allow-other-keys)
  "Draw a rectangle; RADIUS rounds its corners."
  (shape pane "rect" (list :x (number-text x) :y (number-text y)
                           :width (number-text width) :height (number-text height)
                           :rx (and radius (number-text radius)))
         options))

(defun draw-circle (pane x y radius &rest options)
  (shape pane "circle" (list :cx (number-text x) :cy (number-text y) :r (number-text radius))
         options))

(defun draw-ellipse (pane x y x-radius y-radius &rest options)
  (shape pane "ellipse" (list :cx (number-text x) :cy (number-text y)
                              :rx (number-text x-radius) :ry (number-text y-radius))
         options))

(defun draw-arc (pane x y radius start-angle sweep-angle &rest options)
  "Draw the arc of the circle around X, Y that starts at START-ANGLE and
goes on for SWEEP-ANGLE (radians, from the x axis towards the y axis); with
:FILLED, the sector."
  (flet ((point (angle)
           (format nil "~A ~A" (number-text (+ x (* radius (cos angle))))
                   (number-text (+ y (* radius (sin angle)))))))
    (let ((filled (getf options :filled (getf *graphics-state* :filled)))
          (arc (format nil "A~A ~A 0 ~D ~D ~A"
                       (number-text radius) (number-text radius)
                       (if (> (abs sweep-angle) pi) 1 0) (if (minusp sweep-angle) 0 1)
                       (point (+ start-angle sweep-angle)))))
      (shape pane "path"
             (list :d (if filled
                          (format nil "M~A ~A L~A ~A Z" (number-text x) (number-text y)
                                  (point start-angle) arc)
                          (format nil "M~A ~A" (point start-angle) arc)))
             options))))

(defun draw-path (pane data &rest options)
  "Draw the path DATA, in the notation of SVG paths (\"M0 0 L10 10 Z\")."
  (shape pane "path" (list :d data) options))

(defun draw-string (pane string x y &rest options &key font-size font-weight (anchor :start)
                                      (baseline :alphabetic) &allow-other-keys)
  "Draw STRING at X, Y: ANCHOR says whether that is its :START, :MIDDLE or
:END, BASELINE whether it is its :ALPHABETIC baseline, its :MIDDLE or its :TOP."
  (shape pane "text"
         (list :x (number-text x) :y (number-text y)
               :font-size (and font-size (number-text font-size))
               :font-weight font-weight
               :text-anchor (string-downcase anchor)
               :dominant-baseline (ecase baseline
                                    (:alphabetic nil)
                                    (:middle "central")
                                    (:top "hanging")))
         options
         (text-of string)))

;;; ------------------------------------------------------------------
;;; Groups and transformations

(defun call-with-drawing-group (properties function)
  (let ((children (let ((*shapes* '()))
                    (funcall function)
                    (reverse *shapes*))))
    (let ((node (apply #'element "g" properties children)))
      (push node *shapes*)
      node)))

(defmacro with-drawing-group ((pane &key class) &body body)
  "What BODY draws as one shape; its node is returned, so that
  (with-output-as-presentation (object) (with-drawing-group (pane) ...))
makes a presentation of several shapes."
  `(progn ,pane (call-with-drawing-group (list :class ,class) (lambda () ,@body))))

(defmacro with-graphics-translation ((pane dx dy) &body body)
  "Draw what BODY draws moved by DX, DY."
  `(progn ,pane
          (call-with-drawing-group
           (list :transform (format nil "translate(~A ~A)" (number-text ,dx) (number-text ,dy)))
           (lambda () ,@body))))

(defmacro with-graphics-rotation ((pane angle &optional (x 0) (y 0)) &body body)
  "Draw what BODY draws turned by ANGLE (radians) around X, Y."
  `(progn ,pane
          (call-with-drawing-group
           (list :transform (format nil "rotate(~A ~A ~A)"
                                    (number-text (* ,angle (/ 180 pi))) (number-text ,x) (number-text ,y)))
           (lambda () ,@body))))

(defmacro with-graphics-scale ((pane sx &optional (sy sx)) &body body)
  "Draw what BODY draws scaled by SX and SY."
  `(progn ,pane
          (call-with-drawing-group
           (list :transform (format nil "scale(~A ~A)" (number-text ,sx) (number-text ,sy)))
           (lambda () ,@body))))
