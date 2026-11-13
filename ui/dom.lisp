;;;; dom.lisp — the UI backend for the browser's DOM
;;;;
;;;; Loading this file makes it the default backend:
;;;;   (ui:mount (app) "#app")       ; a CSS selector, or a DOM node
;;;;
;;;; Properties are the JavaScript properties of the element with the keyword's
;;;; name in lower case (:VALUE, :HIDDEN, :TITLE, :PLACEHOLDER...), except
;;;; :CLASS (className), :FOR (htmlFor), and :STYLE and names with a hyphen
;;;; (:ARIA-LABEL, :DATA-ID), which are attributes.  Listeners are called with
;;;; the DOM event.
;;;;
;;;; Compile it after ui.lisp and command.lisp.

(in-package "UI")

(defclass dom-backend ()
  ((document :initform (js:global "document") :reader backend-document)
   (flush :initform (js:async-function #'flush)))
  (:documentation "The backend for the DOM of a page; loading dom.lisp makes an
instance of it the value of *BACKEND*."))

;;; the JavaScript strings of names are made once (a string is converted
;;; character by character)
(defmacro define-js-names (&rest names)
  `(progn ,@(loop for (var string) on names by #'cddr
                  collect `(defvar ,var (js:to-js ,string)))))

(define-js-names
  +create-element+ "createElement" +create-text-node+ "createTextNode"
  +node-value+ "nodeValue" +set-attribute+ "setAttribute" +remove-attribute+ "removeAttribute"
  +add-event-listener+ "addEventListener" +insert-before+ "insertBefore"
  +remove+ "remove" +replace-with+ "replaceWith" +next-sibling+ "nextSibling"
  +is-same-node+ "isSameNode" +query-selector+ "querySelector"
  +queue-microtask+ "queueMicrotask")

(defvar *js-names* (make-hash-table :test 'eq) "keyword -> (attributep . JavaScript string)")
(defvar *js-tags* (make-hash-table :test 'equal))

(defun property-name (key)
  "For the property KEY: whether it is an attribute, and its name as a JavaScript string."
  (let ((entry (or (gethash key *js-names*)
                   (setf (gethash key *js-names*)
                         (let ((name (string-downcase (symbol-name key))))
                           (cond ((eq key :class) (cons nil (js:to-js "className")))
                                 ((eq key :for) (cons nil (js:to-js "htmlFor")))
                                 ((eq key :tabindex) (cons nil (js:to-js "tabIndex")))
                                 ((eq key :readonly) (cons nil (js:to-js "readOnly")))
                                 ((eq key :colspan) (cons nil (js:to-js "colSpan")))
                                 ((eq key :role) (cons t (js:to-js "role")))
                                 ;; (the property is read-only)
                                 ((eq key :list) (cons t (js:to-js "list")))
                                 ((or (eq key :style) (find #\- name)) (cons t (js:to-js name)))
                                 (t (cons nil (js:to-js name)))))))))
    (values (car entry) (cdr entry))))

(defun attribute-name (key)
  (case key
    (:class "class")
    (:for "for")
    (t (string-downcase (symbol-name key)))))

;;; the elements of drawings (graphics.lisp) are SVG: they are created in
;;; its namespace, and all their properties are attributes

(defvar *svg-tags* '("svg" "g" "line" "rect" "circle" "ellipse" "polygon" "polyline" "path"
                     "text" "tspan" "image"))
(defvar +svg-namespace+ (js:to-js "http://www.w3.org/2000/svg"))
(defvar *svg-element-p* (js:eval "(node) => node instanceof SVGElement"))

(defun svg-node-p (node)
  (eq (js:funcall *svg-element-p* node) t))

(defun svg-attribute-name (key)
  (if (eq key :viewbox) "viewBox" (attribute-name key)))

(defmethod make-element-node ((backend dom-backend) tag)
  (let ((name (or (gethash tag *js-tags*)
                  (setf (gethash tag *js-tags*) (js:to-js tag)))))
    (if (member tag *svg-tags* :test #'string=)
        (js:call (backend-document backend) "createElementNS" +svg-namespace+ name)
        (js:call (backend-document backend) +create-element+ name))))

(defmethod make-text-node ((backend dom-backend) string)
  (js:call (backend-document backend) +create-text-node+ string))

(defmethod set-node-text ((backend dom-backend) node string)
  (js:set node +node-value+ string))

(defmethod set-property ((backend dom-backend) node name value)
  (multiple-value-bind (attributep js-name) (property-name name)
    (cond (attributep (js:call node +set-attribute+ js-name value))
          ((svg-node-p node) (js:call node +set-attribute+ (svg-attribute-name name) value))
          ;; what the user may have changed: only set a different value
          ;; (setting the value of a text field moves the cursor)
          ((eq name :value)
           (unless (equal (js:get node js-name) value)
             (js:set node js-name value)))
          (t (js:set node js-name value)))))

(defmethod remove-property ((backend dom-backend) node name old-value)
  (multiple-value-bind (attributep js-name) (property-name name)
    (cond (attributep (js:call node +remove-attribute+ js-name))
          ((svg-node-p node) (js:call node +remove-attribute+ (svg-attribute-name name)))
          ((eq name :value) (js:set node js-name ""))
          ((or (eq name :checked) (eq old-value t)) (js:set node js-name :false))
          (t (js:call node +remove-attribute+ (attribute-name name))))))

;;; Listeners are functions of JS:ASYNC-FUNCTION: where the JavaScript
;;; engine can suspend WebAssembly, a listener can wait for the user in the
;;; middle of its work (WAIT-FOR), which is how an error asks which restart
;;; to invoke in a dialog of the page.

(defmethod set-listener ((backend dom-backend) node event function)
  (js:call node +add-event-listener+ event (js:async-function function)))

(defmethod can-wait-p ((backend dom-backend))
  (js:can-await-p))

(defmethod wait-for ((backend dom-backend) function)
  (let* ((answer nil)
         (promise (js:new (js:global "Promise")
                          (lambda (resolve &rest more)
                            (declare (ignore more))
                            (funcall function (lambda (value)
                                                (setf answer value)
                                                (js:funcall resolve nil)
                                                nil))
                            nil))))
    (js:await promise)
    answer))

(defmethod insert-node ((backend dom-backend) parent node before)
  (js:call parent +insert-before+ node before))

(defmethod remove-node ((backend dom-backend) node)
  (js:call node +remove+))

(defmethod replace-node ((backend dom-backend) old new)
  (js:call old +replace-with+ new))

(defmethod node-follows-p ((backend dom-backend) node next)
  (let ((sibling (js:get node +next-sibling+)))
    (if next
        (and sibling (js:call next +is-same-node+ sibling))
        (null sibling))))

(defmethod request-flush ((backend dom-backend))
  ;; after the event handler that changed the state has returned
  (js:call (js:global) +queue-microtask+ (slot-value backend 'flush)))

(defmethod resolve-container ((backend dom-backend) container)
  (if (stringp container)
      (or (js:call (backend-document backend) +query-selector+ container)
          (error "UI: no element matches ~S" container))
      container))

;;; events and nodes, for the panes

(defmethod event-get ((backend dom-backend) event name)
  (js:get event name))

(defmethod event-target-get ((backend dom-backend) event name)
  (js:get (js:get event "target") name))

(defmethod stop-event ((backend dom-backend) event)
  (js:call event "preventDefault")
  (js:call event "stopPropagation"))

(defmethod node-call ((backend dom-backend) node method &rest arguments)
  (apply #'js:call node method arguments))

(defmethod overlay-container ((backend dom-backend))
  (js:get (backend-document backend) "body"))

(defmethod set-global-listener ((backend dom-backend) event function)
  (js:call (backend-document backend) +add-event-listener+ event
           (js:async-function (lambda (event) (funcall function event) nil))))

(defmethod set-title ((backend dom-backend) string)
  (js:set (backend-document backend) "title" string))

(defmethod capture-pointer ((backend dom-backend) event)
  (let ((node (js:get event "currentTarget")))
    (when node (js:call node "setPointerCapture" (js:get event "pointerId")))))

(defmethod viewport-size ((backend dom-backend))
  (values (js:get (js:global) "innerWidth") (js:get (js:global) "innerHeight")))

(defmethod begin-drag ((backend dom-backend) event text)
  ;; (without data, some browsers do not start the drag)
  (let ((transfer (js:get event "dataTransfer")))
    (when transfer
      (js:call transfer "setData" "text/plain" text)
      (js:set transfer "effectAllowed" "move"))))

;;; files and the clipboard

(defvar *choose-file*
  (js:eval "(accept, done) => { const input = document.createElement('input');
     input.type = 'file'; if (accept) input.accept = accept;
     input.addEventListener('cancel', () => done(null, null));
     input.addEventListener('change', () => { const file = input.files[0];
       if (!file) done(null, null); else file.text().then((text) => done(file.name, text), () => done(null, null)); });
     input.click(); }"))

(defmethod choose-file ((backend dom-backend) continuation &key accept)
  (js:funcall *choose-file* accept
              (js:async-function
               (lambda (name text)
                 (let ((*backend* backend))
                   (with-error-dialog (funcall continuation name text))
                   (flush))
                 nil)))
  nil)

(defmethod write-clipboard ((backend dom-backend) text)
  (let ((clipboard (js:get (js:global "navigator") "clipboard")))
    (when clipboard (js:call clipboard "writeText" text))
    text))

(defvar *read-clipboard*
  (js:eval "(done) => { const c = navigator.clipboard;
     if (!c || !c.readText) done(null); else c.readText().then((text) => done(text), () => done(null)); }"))

(defmethod read-clipboard ((backend dom-backend) continuation)
  (js:funcall *read-clipboard*
              (js:async-function
               (lambda (text)
                 (let ((*backend* backend))
                   (with-error-dialog
                     ;; a browser that does not let a page read the clipboard:
                     ;; the user pastes it
                     (if (and (null text) *paste-function*)
                         (funcall *paste-function* continuation)
                         (funcall continuation text)))
                   (flush))
                 nil)))
  nil)

(defvar *reveal-selection*
  (js:eval "(event) => { const node = event.currentTarget || event.target;
     queueMicrotask(() => { const item = node.querySelector('.selected');
                            if (item) item.scrollIntoView({ block: 'nearest', inline: 'nearest' }); }); }"))

(defmethod reveal-selection ((backend dom-backend) event)
  ;; the display is brought up to date in a microtask that is already queued
  (when event (js:funcall *reveal-selection* event))
  nil)

(defvar *event-position*
  (js:eval "(event) => { let node = event.currentTarget || event.target;
     if (!(node instanceof SVGSVGElement)) node = node.ownerSVGElement || node.querySelector('svg');
     const p = new DOMPoint(event.clientX, event.clientY).matrixTransform(node.getScreenCTM().inverse());
     return [p.x, p.y]; }"))

(defmethod event-position ((backend dom-backend) event)
  (let ((position (js:funcall *event-position* event)))
    (values (js:get position 0) (js:get position 1))))

(defmethod previous-sibling-size ((backend dom-backend) event)
  (let* ((target (js:get event "target"))
         (previous (js:get target "previousElementSibling")))
    (js:call target "setPointerCapture" (js:get event "pointerId"))
    (values (js:get previous "offsetWidth") (js:get previous "offsetHeight"))))

;;; errors: the browser's dialogs

(defmethod choose-restart ((backend dom-backend) condition restarts)
  (let ((message (princ-to-string condition))
        (window (js:global)))
    (js:call (js:global "console") "error" message)
    (cond
      ((null (cdr restarts))
       (if *error-message-function*
           (funcall *error-message-function* message)
           (js:call window "alert" message))
       nil)
      ;; in a dialog of the page, if this computation can wait for the answer
      ((and *restart-chooser* (can-wait-p backend))
       (funcall *restart-chooser* condition restarts))
      (t
        (let ((answer (js:call window "prompt"
                               (format nil "~A~%~%~{~D: ~A~%~}" message
                                       (loop for restart in restarts for i from 1
                                             collect i collect (princ-to-string restart)))
                               "1")))
          (and (stringp answer)
               (let ((n (parse-integer answer :junk-allowed t)))
                 (and n (<= 1 n (length restarts)) (nth (1- n) restarts)))))))))

;;; commands: their keys

(defun event-key (event)
  "The key of the keyboard event EVENT in the form of PARSE-KEY."
  (cons (string-upcase (js:get event "key"))
        (append (and (js:get event "altKey") '(:alt))
                (and (or (js:get event "ctrlKey") (js:get event "metaKey")) '(:ctrl))
                (and (js:get event "shiftKey") '(:shift)))))


(defun install-command-keys (backend &key (command-predicate (constantly t)) (handlers t))
  "Invoke the commands that satisfy COMMAND-PREDICATE when their keys are
pressed, and (if HANDLERS) what else has keys, like the items of menus."
  (js:call (backend-document backend) +add-event-listener+ "keydown"
           (js:async-function
            (lambda (event)
              (let ((action (and (stringp (js:get event "key"))
                                 (let ((*backend* backend))
                                   (key-action (event-key event)
                                               :command-predicate command-predicate
                                               :handlers handlers)))))
                (when action
                  (js:call event "preventDefault")
                  (let ((*backend* backend))
                    (with-error-dialog (funcall action)))))
              nil))))

(defmethod install-commands ((backend dom-backend) &key &allow-other-keys)
  (install-command-keys backend))

(setf *backend* (make-instance 'dom-backend))
