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
   ;; (see VIEW-WIDTH)
   (view-height :initarg :view-height :initform nil :accessor output-pane-view-height)
   ;; functions of the pane and the position X, Y in the drawing where the
   ;; pointer was pressed, moved and released
   (press-callback :initarg :press-callback :initform nil :accessor output-pane-press-callback)
   ;; called with the pane and X, Y when the pointer moves on the drawing
   (motion-callback :initarg :motion-callback :initform nil :accessor output-pane-motion-callback)
   ;; called with the pane and X, Y when the pointer is released
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
  "Draw a line from X1, Y1 to X2, Y2."
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
  "Draw a circle around X, Y; with :FILLED, a disc."
  (shape pane "circle" (list :cx (number-text x) :cy (number-text y) :r (number-text radius))
         options))

(defun draw-ellipse (pane x y x-radius y-radius &rest options)
  "Draw an ellipse around X, Y with the radii X-RADIUS and Y-RADIUS."
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

(defun draw-image (pane url x y width height &rest options)
  "Draw the image at URL in the rectangle X, Y, WIDTH, HEIGHT."
  (declare (ignore pane))
  (unless *drawing*
    (error "UI: a drawing function was called outside the DISPLAY-CALLBACK of an OUTPUT-PANE"))
  (let ((node (element "image" (list :href url :x (number-text x) :y (number-text y)
                                     :width (number-text width) :height (number-text height)
                                     :opacity (and (getf options :opacity) (number-text (getf options :opacity)))
                                     :class (getf options :class)))))
    (push node *shapes*)
    node))

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

;;; ------------------------------------------------------------------
;;; Graphs (the graph pane of CAPI, FORMAT-GRAPH-FROM-ROOTS of CLIM)

(defmodel graph-pane (choice)
  (;; a function of an item that returns the items its edges lead to
   (children-function :initarg :children-function :initform (constantly nil)
                      :accessor graph-pane-children-function)
   ;; :LEFT-RIGHT or :TOP-DOWN
   (direction :initarg :direction :initform :left-right :accessor graph-pane-direction))
  (:default-initargs :stretch t)
  (:documentation "The items :ROOTS and what CHILDREN-FUNCTION returns for them, as
boxes joined by edges.  An item that is reached several times is shown once.
The items are selected and activated as in a list, and are presentations if
the pane has a :PRESENTATION-TYPE."))

(defmethod initialize-instance :after ((pane graph-pane) &key (roots nil rootsp))
  (when rootsp (setf (collection-items pane) roots)))

(defun graph-pane-roots (pane)
  "The items the graph PANE starts from (its COLLECTION-ITEMS)."
  (collection-items pane))
(defun (setf graph-pane-roots) (roots pane)
  (setf (collection-items pane) roots))

(defun text-width (string)
  "About how wide STRING is, in pixels of the font of the panes."
  (let ((width 0))
    (dotimes (i (length string) width)
      (incf width (if (< (char-code (char string i)) 768) 7.2 13)))))

(defun layout-graph (pane)
  "The nodes of the graph as (item label depth position) and its edges as
(from . to), the positions being rows of leaves."
  (let* ((test (collection-test pane))
         (nodes '()) (edges '()) (next 0)
         ;; depth -> the position of the lowest node of that depth so far
         (lowest (make-hash-table)))
    (labels ((visit (item depth)
               (let ((node (find item nodes :key #'first :test test)))
                 (or node
                     (let ((node (list item (item-text pane item) depth 0)))
                       (push node nodes)
                       (let ((children (mapcar (lambda (child)
                                                 (let ((child-node (visit child (1+ depth))))
                                                   (push (cons node child-node) edges)
                                                   child-node))
                                               (funcall (graph-pane-children-function pane) item))))
                         ;; a node is in the middle of the nodes it leads to (that are deeper)
                         (let* ((below (remove-if-not (lambda (child) (> (third child) depth)) children))
                                (position (if below
                                              (/ (+ (reduce #'min below :key #'fourth)
                                                    (reduce #'max below :key #'fourth))
                                                 2)
                                              next))
                                ;; not on top of a node of the same depth
                                (last (gethash depth lowest)))
                           (when (and last (< position (1+ last)))
                             (setf position (1+ last)))
                           (setf (gethash depth lowest) position
                                 (fourth node) position
                                 next (max next (1+ (floor position))))))
                       node)))))
      (dolist (root (collection-items pane)) (visit root 0)))
    (values (reverse nodes) (reverse edges))))

(defmethod choice-visible-items ((pane graph-pane))
  (mapcar #'first (layout-graph pane)))

(defmethod render-pane ((pane graph-pane))
  (multiple-value-bind (nodes edges) (layout-graph pane)
    (let* ((horizontal (not (eq (graph-pane-direction pane) :top-down)))
           (height 26) (gap 12) (level-gap 44)
           (depths (1+ (reduce #'max nodes :key #'third :initial-value 0)))
           ;; the width of the boxes of every depth (top-down: of all)
           (widths (loop for depth below depths
                         collect (+ 20 (reduce #'max (remove depth nodes :key #'third :test-not #'eql)
                                               :key (lambda (node) (text-width (second node)))
                                               :initial-value 20))))
           (widest (reduce #'max widths :initial-value 0))
           (shapes '()))
      (labels ((box (node)
                 ;; x y width height
                 (let ((depth (third node)) (position (fourth node)))
                   (if horizontal
                       (list (+ 8 (loop for i below depth sum (+ (nth i widths) level-gap)))
                             (+ 8 (* position (+ height gap)))
                             (nth depth widths) height)
                       (list (+ 8 (* position (+ widest gap)))
                             (+ 8 (* depth (+ height level-gap)))
                             widest height))))
               (edge (from to)
                 (destructuring-bind (x1 y1 w1 h1) (box from)
                   (destructuring-bind (x2 y2 w2 h2) (box to)
                     (multiple-value-bind (ax ay bx by)
                         (if horizontal
                             (values (+ x1 w1) (+ y1 (/ h1 2)) x2 (+ y2 (/ h2 2)))
                             (values (+ x1 (/ w1 2)) (+ y1 h1) (+ x2 (/ w2 2)) y2))
                       (let ((mx (/ (+ ax bx) 2)) (my (/ (+ ay by) 2)))
                         (element "path"
                                  (list :class "ui-graph-edge" :fill "none"
                                        :d (if horizontal
                                               (format nil "M~A ~A C~A ~A ~A ~A ~A ~A"
                                                       (number-text ax) (number-text ay)
                                                       (number-text mx) (number-text ay)
                                                       (number-text mx) (number-text by)
                                                       (number-text bx) (number-text by))
                                               (format nil "M~A ~A C~A ~A ~A ~A ~A ~A"
                                                       (number-text ax) (number-text ay)
                                                       (number-text ax) (number-text my)
                                                       (number-text bx) (number-text my)
                                                       (number-text bx) (number-text by)))))))))))
        (dolist (edge edges) (push (edge (car edge) (cdr edge)) shapes))
        (dolist (node nodes)
          (destructuring-bind (x y width height) (box node)
            (let* ((item (first node))
                   (selected (item-selected-p pane item)))
              (push (item-element
                     pane item
                     (element "g"
                              (list :class (if selected "ui-graph-node selected" "ui-graph-node")
                                    :on-click (lambda (&optional event) (click-item pane item event))
                                    :on-dblclick (lambda (&optional event)
                                                   (declare (ignore event))
                                                   (activate-item pane item)))
                              (element "rect" (list :x (number-text x) :y (number-text y)
                                                    :width (number-text width) :height (number-text height)
                                                    :rx "5"))
                              (element "text" (list :x (number-text (+ x (/ width 2)))
                                                    :y (number-text (+ y (/ height 2)))
                                                    :text-anchor "middle" :dominant-baseline "central")
                                       (second node))))
                    shapes))))
        (let ((total-width (+ 16 (if horizontal
                                     (+ (reduce #'+ widths) (* level-gap (1- depths)))
                                     (reduce #'max (mapcar (lambda (node) (+ (first (box node)) widest)) nodes)
                                             :initial-value 0))))
              (total-height (+ 16 (reduce #'max (mapcar (lambda (node)
                                                          (+ (second (box node)) height))
                                                        nodes)
                                          :initial-value 0))))
          (html
            (:div :class "ui-graph-pane" :tabindex (if (pane-enabled pane) 0 -1)
                  :on-keydown (lambda (&optional event)
                                (move-selection pane (event-get *backend* event "key") event))
              (element "svg" (list :width (number-text total-width) :height (number-text total-height))
                       (reverse shapes)))))))))
