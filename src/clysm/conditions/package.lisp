;;;; package.lisp - Package definition for clysm/conditions
;;;; ANSI Common Lisp Condition System implementation

(in-package #:cl-user)

(defpackage #:clysm/conditions
  (:use #:cl)
  (:nicknames #:clysm.conditions)
  ;; Shadow CL symbols we need to redefine for condition system
  (:shadow #:condition #:error #:warning
           #:serious-condition
           #:simple-condition #:simple-error #:simple-warning
           #:type-error #:cell-error #:unbound-variable #:undefined-function
           #:control-error #:program-error
           #:signal #:warn #:cerror
           #:restart #:find-restart #:compute-restarts #:invoke-restart
           #:invoke-restart-interactively #:invoke-debugger
           #:abort #:continue #:muffle-warning #:use-value #:store-value
           #:simple-condition-format-control #:simple-condition-format-arguments
           #:type-error-datum #:type-error-expected-type #:cell-error-name
           #:make-condition #:restart-name
           #:handler-case #:handler-bind
           #:restart-case #:restart-bind #:with-simple-restart)
  ;; Export our shadowed definitions
  (:export
   ;; Base condition classes (T008-T019)
   #:condition
   #:serious-condition
   #:error
   #:warning
   #:simple-condition
   #:simple-error
   #:simple-warning
   #:type-error
   #:cell-error
   #:unbound-variable
   #:undefined-function
   #:control-error
   ;; Condition accessors
   #:simple-condition-format-control
   #:simple-condition-format-arguments
   #:type-error-datum
   #:type-error-expected-type
   #:cell-error-name
   ;; Condition construction
   #:make-condition
   ;; Signaling functions (T027, T031, T051, T057, T081)
   #:signal
   #:warn
   #:error
   #:cerror
   ;; Handler macros (T033, T063)
   #:handler-case
   #:handler-bind
   ;; Restart system (T041-T047, T080)
   #:restart
   #:restart-name
   #:find-restart
   #:compute-restarts
   #:invoke-restart
   #:invoke-restart-interactively
   #:restart-case
   #:restart-bind
   #:with-simple-restart
   ;; Standard restarts (T052, T058, T070-T072)
   #:abort
   #:continue
   #:muffle-warning
   #:use-value
   #:store-value))

(defpackage #:clysm/conditions/types
  (:use #:cl)
  (:documentation "Condition type hierarchy definitions"))

(defpackage #:clysm/conditions/handlers
  (:use #:cl)
  (:documentation "Handler establishment macros"))

(defpackage #:clysm/conditions/restarts
  (:use #:cl)
  (:documentation "Restart system implementation"))

(defpackage #:clysm/conditions/signaling
  (:use #:cl)
  (:documentation "Condition signaling functions"))

(defpackage #:clysm/conditions/standard
  (:use #:cl)
  (:documentation "Standard restart implementations"))
