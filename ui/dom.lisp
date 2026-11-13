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

(in-package "UI")

(defclass dom-backend ()
  ((document :initform (js:global "document") :reader backend-document)
   (flush :initform (js:to-js #'flush))))

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
                                 ((or (eq key :style) (find #\- name)) (cons t (js:to-js name)))
                                 (t (cons nil (js:to-js name)))))))))
    (values (car entry) (cdr entry))))

(defun attribute-name (key)
  (case key
    (:class "class")
    (:for "for")
    (t (string-downcase (symbol-name key)))))

(defmethod make-element-node ((backend dom-backend) tag)
  (js:call (backend-document backend) +create-element+
           (or (gethash tag *js-tags*)
               (setf (gethash tag *js-tags*) (js:to-js tag)))))

(defmethod make-text-node ((backend dom-backend) string)
  (js:call (backend-document backend) +create-text-node+ string))

(defmethod set-node-text ((backend dom-backend) node string)
  (js:set node +node-value+ string))

(defmethod set-property ((backend dom-backend) node name value)
  (multiple-value-bind (attributep js-name) (property-name name)
    (cond (attributep (js:call node +set-attribute+ js-name value))
          ;; what the user may have changed: only set a different value
          ;; (setting the value of a text field moves the cursor)
          ((eq name :value)
           (unless (equal (js:get node js-name) value)
             (js:set node js-name value)))
          (t (js:set node js-name value)))))

(defmethod remove-property ((backend dom-backend) node name old-value)
  (multiple-value-bind (attributep js-name) (property-name name)
    (cond (attributep (js:call node +remove-attribute+ js-name))
          ((eq name :value) (js:set node js-name ""))
          ((or (eq name :checked) (eq old-value t)) (js:set node js-name :false))
          (t (js:call node +remove-attribute+ (attribute-name name))))))

(defmethod set-listener ((backend dom-backend) node event function)
  (js:call node +add-event-listener+ event function))

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

(setf *backend* (make-instance 'dom-backend))
