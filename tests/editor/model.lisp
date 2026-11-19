;;;; tests/editor/model.lisp — tests of the text, commands and syntax of
;;;; the editor component (ui/editor/{buffer,syntax,commands}.lisp)
;;;;
;;;; Portable: run on Clysm with tests/editor/run.sh, or on SBCL:
;;;;   sbcl --script tests/editor/sbcl.lisp

(defpackage "EDITOR-TEST" (:use "CL" "EDITOR"))
(in-package "EDITOR-TEST")

(defvar *failures* 0)
(defvar *count* 0)

(defmacro check (name form expected)
  `(let ((value (handler-case ,form (error (e) (list :error (princ-to-string e))))))
     (incf *count*)
     (unless (equalp value ,expected)
       (incf *failures*)
       (format t "FAIL ~A~%  got:      ~S~%  expected: ~S~%" ,name value ,expected))))

(defun mk (text &key (language *lisp-language*) (keymap *standard-keymap*) cursor raw)
  "An editor with TEXT; | in it is the cursor (unless RAW)."
  (let* ((bar (and (not raw) (position #\| text)))
         (e (make-editor :text (if bar (remove #\| text :count 1) text) :language language :keymap keymap)))
    (when (or bar cursor)
      (set-selection e (or cursor (offset-pos (editor-document e) bar))))
    e))

(defun show (e)
  "The text of E with | at the cursor (and [ ] around a selection)."
  (let* ((doc (editor-document e))
         (text (editor-text e))
         (h (pos-offset doc (editor-head e)))
         (a (pos-offset doc (editor-anchor e))))
    (if (= h a)
        (concatenate 'string (subseq text 0 h) "|" (subseq text h))
        (let ((s (min a h)) (x (max a h)))
          (concatenate 'string (subseq text 0 s) "[" (subseq text s x) "]" (subseq text x))))))

(defun keys (e &rest keys)
  (dolist (k keys e)
    (if (and (> (length k) 1) (char= (char k 0) #\"))
        (type-text e (subseq k 1))
        (handle-key e k))))

;;; the document
(check "split" (editor-text (mk (format nil "a~Cb~C~Cc" #\Return #\Return #\Newline)))
       (format nil "a~%b~%c"))
(let ((e (mk "abc")))
  (insert-text e (make-pos 0 1) (format nil "X~%Y~%Z"))
  (check "insert lines" (editor-text e) (format nil "aX~%Y~%Zbc"))
  (check "line count" (line-count (editor-document e)) 3)
  (delete-range e (make-pos 0 1) (make-pos 2 1))
  (check "delete lines" (editor-text e) "abc")
  (undo e)
  (check "undo delete" (editor-text e) (format nil "aX~%Y~%Zbc"))
  (undo e)
  (check "undo insert" (editor-text e) "abc")
  (redo e)
  (check "redo" (editor-text e) (format nil "aX~%Y~%Zbc")))

;;; typing, undo grouping
(let ((e (mk "|")))
  (keys e "\"h" "\"e" "\"l" "\"l" "\"o")
  (check "typed" (show e) "hello|")
  (undo e)
  (check "typing is one undo step" (show e) "|"))

(let ((e (mk "|")))
  (keys e "\"(" "\"a" "\")" "\" " "\"b")
  (undo e)
  (check "typing over a bracket is in the same undo step" (show e) "|"))

(check "auto pair" (show (keys (mk "|") "\"(")) "(|)")
(check "type over close" (show (keys (mk "|") "\"(" "\"a" "\")")) "(a)|")
(check "backspace pair" (show (keys (mk "|") "\"(" "Backspace")) "|")
(check "wrap selection" (show (keys (mk "abc") "Mod-a" "\"(")) "([abc])")
(check "string pair" (show (keys (mk "|") "\"\"")) "\"|\"")
(check "close string" (show (keys (mk "|") "\"\"" "\"x" "\"\"")) "\"x\"|")

;;; motion
(check "word" (show (keys (mk "|foo-bar baz") "C-Right")) "foo-bar| baz")
(check "word back" (show (keys (mk "foo bar|") "C-Left")) "foo |bar")
(check "home toggles" (show (keys (mk "   ab|c") "Home")) "   |abc")
(check "home again" (show (keys (mk "   ab|c") "Home" "Home")) "|   abc")
(check "goal column" (show (keys (mk (format nil "abcdef|~%x~%abcdefgh")) "Down" "Down"))
       (format nil "abcdef~%x~%abcdef|gh"))
(check "select" (show (keys (mk "a|bcd") "S-Right" "S-Right")) "a[bc]d")
(check "collapse" (show (keys (mk "a|bcd") "S-Right" "S-Right" "Left")) "a|bcd")
(check "emacs" (show (keys (mk (format nil "ab|c~%de") :keymap *emacs-keymap*) "C-n" "C-a" "C-f"))
       (format nil "abc~%d|e"))
(check "emacs mark" (show (keys (mk "|abc" :keymap *emacs-keymap*) "C-Space" "C-f" "C-f")) "[ab]c")
(check "emacs prefix" (show (keys (mk "a|b" :keymap *emacs-keymap*) "C-x" "h")) "[ab]")

;;; kill and yank
(let ((e (mk (format nil "|abc~%def") :keymap *emacs-keymap*)))
  (keys e "C-k" "C-k")
  (check "kill line twice" (show e) "|def")
  (keys e "C-e" "C-y")
  (check "yank appended kills" (show e) (format nil "defabc~%|")))

;;; editing
(check "newline indents"
       (show (keys (mk "(defun foo (x)|") "Return"))
       (format nil "(defun foo (x)~%  |"))
(check "newline under argument"
       (show (keys (mk "(foo a|") "Return"))
       (format nil "(foo a~%     |"))
(check "newline in let bindings"
       (show (keys (mk "(let ((a 1)|") "Return"))
       (format nil "(let ((a 1)~%      |"))
(check "newline in flet"
       (show (keys (mk "(flet ((f (x)|") "Return"))
       (format nil "(flet ((f (x)~%         |"))
(check "data list"
       (show (keys (mk "'(a|") "Return"))
       (format nil "'(a~%  |"))
(check "toplevel" (show (keys (mk "(a)|") "Return")) (format nil "(a)~%|"))
(check "paren in string ignored"
       (show (keys (mk "(foo \"(\" |") "Return"))
       (format nil "(foo \"(\"~%     |"))
(check "paren in comment ignored"
       (show (keys (mk (format nil "(a ; (~%|b")) "Tab"))
       (format nil "(a ; (~% |b"))
(check "tab indents"
       (show (keys (mk (format nil "(defun f ()~%|x)")) "Tab"))
       (format nil "(defun f ()~%  |x)"))
(check "plain tab" (show (keys (mk "a|" :language *plain-text*) "Tab")) "a |")
(check "plain newline keeps indent"
       (show (keys (mk "  foo|" :language *plain-text*) "Return"))
       (format nil "  foo~%  |"))
(check "braces" (show (keys (mk "x {|}" :language *plain-text*) "Return"))
       (format nil "x {~%  |~%}"))
(check "comment" (show (keys (mk (format nil "(a|~% b)")) "Mod-a" "Mod-/"))
       (format nil "[;; (a~%;;  b)]"))
(check "uncomment" (show (keys (mk (format nil ";; (a|")) "Mod-/")) "(a|")
(check "move line up" (show (keys (mk (format nil "a~%b|~%c")) "M-Up")) (format nil "b|~%a~%c"))
(check "move line down" (show (keys (mk (format nil "a|~%b~%c")) "M-Down")) (format nil "b~%a|~%c"))
(check "duplicate" (show (keys (mk (format nil "a|~%b")) "M-S-Down")) (format nil "a~%a|~%b"))
(check "delete word" (show (keys (mk "foo bar|") "C-Backspace")) "foo |")
(check "read-only"
       (let ((e (mk "ab|")))
         (setf (editor-read-only e) t)
         (keys e "\"x" "Backspace")
         (show e))
       "ab|")

;;; S-expressions and brackets
(check "forward sexp" (show (keys (mk "|(a (b) \"c)\") d") "C-M-Right")) "(a (b) \"c)\")| d")
(check "backward sexp" (show (keys (mk "x '(a (b))|") "C-M-Left")) "x |'(a (b))")
(check "forward atom" (show (keys (mk "(|foo bar)") "C-M-Right")) "(foo| bar)")
(check "forward string" (show (keys (mk "(|\"a b\" c)") "C-M-Right")) "(\"a b\"| c)")
(let ((e (mk (format nil "(a~% (b \")\")~% c)"))))
  (check "matching" (matching-bracket e (make-pos 0 0)) (make-pos 2 2))
  (check "matching back" (matching-bracket e (make-pos 2 2)) (make-pos 0 0)))

;;; highlighting
(let ((e (mk (format nil "(defun foo (x) ; c~%  \"s~%t\" :k 12 #\\( #| b |# t)") :raw t)))
  (check "tokens 0" (mapcar #'third (line-tokens e 0))
         '("paren paren-0" "builtin" "def-name" "paren paren-1" "paren paren-1" "comment"))
  (check "tokens 1" (mapcar #'third (line-tokens e 1)) '("string"))
  (check "tokens 2" (mapcar #'third (line-tokens e 2))
         '("string" "keyword" "number" "char" "comment" "constant" "paren paren-0")))
(let ((e (mk (format nil "#| a~%b |# x") :raw t)))
  (check "block comment" (mapcar #'third (line-tokens e 1)) '("comment")))

;;; the state of lines is recomputed after a change
(let ((e (mk (format nil "a~%b~%c"))))
  (line-tokens e 2)
  (insert-text e (make-pos 0 0) "\"")
  (check "string opened above" (mapcar #'third (line-tokens e 2)) '("string")))

;;; search
(let ((e (mk "Foo bar foo")))
  (check "search" (multiple-value-list (search-forward e "foo" :from (make-pos 0 1)))
         (list (make-pos 0 8) (make-pos 0 11)))
  (check "search case" (multiple-value-list (search-forward e "Foo" :from (make-pos 0 1)))
         (list (make-pos 0 0) (make-pos 0 3))))

;;; modified
(let ((e (mk "x")))
  (check "fresh" (editor-modified e) nil)
  (keys e "\"y")
  (check "modified" (editor-modified e) t)
  (mark-saved e)
  (check "saved" (editor-modified e) nil)
  (keys e "\"z")
  (undo e)
  (check "back to saved" (editor-modified e) nil))

(format t "~D tests, ~D failures~%" *count* *failures*)
