;;; A library for tests/lib/package.mjs: what a package of JavaScript exports,
;;; written in Lisp: constants, classes, errors thrown as JavaScript has them,
;;; the types of its .d.ts and modules of their own (entries).
;;; tests/lib/lazy.lisp loads this file in a lazy library.

(defvar *loads* 0)
(incf *loads*)

(js:declare-types "/** Where a thing is. */
export interface Place { kind: 'line'; line: number }")
(js:declare-types "import type { Place } from './package.js';
export type Places = Place[];" :entry "places")

(js:export "version" 113 :type "number")
(js:export "names" #("a" "b") :type "readonly string[]")
(js:export "loads" (lambda () *loads*) :type "() => number")

;; An error of its own: `new PlaceError(reason, message)` from JavaScript, or
;; thrown by Lisp; `instanceof` holds both ways.
(defparameter *place-error*
  (js:define-class "PlaceError"
    :extends "Error"
    :super-arguments '(1)
    :constructor (lambda (this reason message)
                   (declare (ignore message))
                   (js:set this "name" "PlaceError")
                   (js:set this "reason" reason))
    :type "declare class PlaceError extends Error {
  readonly reason: string;
  constructor(reason: string, message: string);
}"))
(js:export "PlaceError" *place-error*)

;; A class whose instances keep a Lisp object.
(defstruct counter (count 0))
(defparameter *counter*
  (js:define-class "Counter"
    :constructor (lambda (this start)
                   (js:set this "state" (make-counter :count (or start 0))))
    :methods (list "add" (lambda (this n)
                           (incf (counter-count (js:get this "state")) n)))
    :getters (list "count" (lambda (this) (counter-count (js:get this "state"))))
    :static (list "of" (lambda (n) (js:new *counter* n)))
    :type "declare class Counter {
  constructor(start?: number);
  add(n: number): number;
  get count(): number;
  static of(n: number): Counter;
}"))
(js:export "Counter" *counter*)
;; the same class under another name, and exports named as the helpers of a lazy library's modules
(js:export "Tally" *counter*)
;; a class of no export of its own name, under two
(let ((hidden (js:define-class "Hidden" :type "declare class Hidden { readonly hidden: true; readonly kind: 'Hidden'; again(): Hidden }"
                               :methods (list "again" (lambda (this) this))
                               :constructor (lambda (this) (js:set this "hidden" t)))))
  (js:export "First" hidden)
  (js:export "Second" hidden))
;; a class named as a property of Object, exported under a name that is no identifier
(js:export "to-string" (js:define-class "toString" :constructor (lambda (this) (js:set this "made" t))
                                       :type "declare class toString { readonly made: true }"))
(js:export "library" (lambda () "a function named library") :type "() => string")
(js:export "classes" "a constant named classes" :type "string")

;; A subclass of a class of the library.
(js:export "LineError"
           (js:define-class "LineError" :extends "PlaceError" :super-arguments '(0 1)
                            :type "declare class LineError extends PlaceError {}"))

(defun parse-line (text)
  (let ((n (parse-integer text :junk-allowed t)))
    (cond ((null n)
           (js:throw (js:new *place-error* "not-a-line" (format nil "[PLACE-001] not a line: ~A" text))))
          ((< n 1)
           ;; a plain Error, as `throw new Error(...)` makes
           (js:throw (js:new (js:global "Error") "[PLACE-002] lines are counted from 1")))
          (t (js:object "kind" "line" "line" n)))))
(js:export "parseLine" #'parse-line :type "(text: string) => Place")

;; What a function of JavaScript throws goes out as it was thrown, also
;; through a handler that declines it, and through an export that waits.
(js:export "callThrough" (lambda (f) (js:funcall f)) :type "(f: () => unknown) => unknown")
(js:export "callHandled"
           (lambda (f)
             (handler-case (js:funcall f)
               (js:js-error (e) (format nil "handled: ~A" (js:js-error-message e)))))
           :type "(f: () => unknown) => unknown")
(js:export "throwValue" (lambda (kind)
                          (js:throw (cond ((equal kind "undefined") :undefined)
                                          ((equal kind "false") :false)
                                          (t 7))))
           :type "(kind: string) => never")
(js:export "awaitThrough" (lambda (f) (js:await (js:funcall f))) :async t
           :type "(f: () => Promise<unknown>) => Promise<unknown>")
;; a Lisp error is still a LispError
(defun check-in-lisp (x) (error "a Lisp error ~A" x))
(defun fails-in-lisp () (check-in-lisp 1) nil)
(js:export "lispError" #'fails-in-lisp :type "() => never")

(js:export "firstPlace" (lambda (places) (js:get places 0)) :entry "places"
           :type "(places: Places) => Place")
(js:export "placesVersion" 2 :entry "places" :type "number")
;; an entry of types alone, and a class of no declaration of its own
(js:declare-types "export interface Point { x: number }" :entry "types")
(js:export "Widget" (js:define-class "Widget" :constructor (lambda (this n) (js:set this "n" n))))
;; (an entry's export named as the binding its eager module had)
(js:export "lib" "a constant named lib" :entry "places" :type "string")
;; written to the standard output: on the standard error, with :output :error
(js:export "say" (lambda (text) (write-line text) (finish-output) nil) :type "(text: string) => null")

;; Arguments as JavaScript has them (:RAW): undefined, null and false told apart, a string read
;; where it is, a value handed back as it came.
(js:export "kinds" (lambda (&rest values)
                     (mapcar (lambda (v) (string-downcase (symbol-name (js:typeof v)))) values))
           :raw t :type "(...values: unknown[]) => string[]")
(js:export "indexIn" (lambda (text needle) (js:call text "indexOf" needle)) :raw t
           :type "(text: string, needle: string) => number")
(js:export "identity" (lambda (value) value) :raw t :type "<T>(value: T) => T")
(js:export "identityLater" (lambda (value) (js:await (js:call (js:global "Promise") "resolve" 1)) value)
           :raw t :async t :type "<T>(value: T) => Promise<T>")
(js:export "Box" (js:define-class "Box" :raw t
                                  :constructor (lambda (this value) (js:set this "kind" (string-downcase (symbol-name (js:typeof value)))))
                                  :methods (list "same" (lambda (this value) (declare (ignore this)) value))
                                  :type "declare class Box { readonly kind: string; same<T>(value: T): T }"))

;; a class exported with a type of its own
(js:export "Typed" (js:define-class "Typed") :type "{ new (n: number): { n: number }; readonly kind: 'typed' }")

;; one function exported converting its arguments and not (:RAW makes a function of its own)
(let ((f (lambda (x) (if (numberp x) "number" "other"))))
  (js:export "converted" f :type "(x: unknown) => string")
  (js:export "unconverted" f :raw t :type "(x: unknown) => string"))
;; a constant of a key __proto__
(js:export "protoData" (js:call (js:global "JSON") "parse" "{\"__proto__\":{\"enabled\":true}}") :type "object")

;; exports named as what the modules use (JSON, process), and as a class exported under other names
(js:export "JSON" "json" :type "string")
(js:export "process" "process" :type "string")
(js:export "Hidden" 123 :type "number")

;; Values as JavaScript has them: JS:GET-RAW, JS:CALL-RAW, JS:FUNCALL-RAW, JS:RAW-FUNCTION.
(flet ((kind (v) (string-downcase (symbol-name (js:typeof v)))))
  (js:export "rawKinds"
             (lambda (o f)
               (list (kind (js:get-raw o "u")) (kind (js:get-raw o "n")) (kind (js:get-raw o "f"))
                     (kind (js:call-raw o "itself")) (kind (js:funcall-raw f))
                     ;; a callback given its arguments unconverted
                     (js:funcall f (js:raw-function (lambda (x) (kind x))))))
             :type "(o: object, f: (callback?: (x: unknown) => string) => unknown) => string[]")
  ;; a part of a long text parsed where it is: the text and the part stay JavaScript's
  (js:export "parsePart"
             (lambda (text start end)
               (js:call (js:global "JSON") "parse" (js:call-raw text "slice" start end)))
             :raw t :type "(text: string, start: number, end: number) => unknown"))
(js:export "awaitKind"
           (lambda (promise) (string-downcase (symbol-name (js:typeof (js:await-raw promise)))))
           :async t :type "(promise: Promise<unknown>) => Promise<string>")
(js:export "awaitLisp" (lambda () (js:await-raw (js:async (lambda () 42)))) :async t
           :type "() => Promise<number>")
(js:export "awaitLispObject"
           (lambda () (js:js-value-p (js:await-raw (js:call (js:global "Promise") "resolve" (js:to-js (make-hash-table))))))
           :async t :type "() => Promise<boolean>")
(js:export "thrownKind"
           (lambda (f)
             (handler-case (progn (js:funcall f) "nothing")
               (js:js-error (e) (string-downcase (symbol-name (js:typeof (js:js-error-thrown e)))))))
           :type "(f: () => unknown) => string")
