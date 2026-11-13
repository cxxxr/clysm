# Apply the Clysm port to an upstream single-file asdf.lisp (3.3.x):
#   curl -o asdf.lisp https://asdf.common-lisp.dev/archives/asdf-3.3.7.lisp && python3 port.py asdf.lisp
import re, sys
p = sys.argv[1]
s = open(p).read()

def rep(old, new, count=1):
    global s
    n = s.count(old)
    assert n >= count, (n, old[:120])
    s = s.replace(old, new, count)

# 1. implementation check
s = re.sub(r'#-\(or abcl allegro clasp clisp (clozure[^)]*)\)\n\(error "ASDF is not supported',
           r'#-(or abcl allegro clasp clisp clysm \1)\n(error "ASDF is not supported', s, count=1)
assert '#-(or abcl allegro clasp clisp clysm clozure' in s

def before_notimpl(name_regex, clysm_line):
    """Insert CLYSM_LINE before the '#-(or ...)' line that precedes
    (not-implemented-error 'NAME ...), and add clysm to that feature list."""
    global s
    m = re.search(r"\n([ \t]*)(#-\(or [^)\n]*\))([ \t]*\n[ \t]*)\(not-implemented-error " + name_regex, s)
    assert m, name_regex
    indent, feat = m.group(1), m.group(2)
    newfeat = feat.replace('#-(or ', '#-(or clysm ', 1)
    s = s[:m.start()] + "\n" + indent + clysm_line + "\n" + indent + newfeat + m.group(3) + s[m.end(3):]

before_notimpl(r"'getenv\)", "#+clysm (sys::%getenv x)")
before_notimpl(r"'delete-empty-directory", "#+clysm (sys::%delete-directory directory-pathname)")
before_notimpl(r"'quit ", "#+clysm (progn (when finish-output (finish-outputs)) (sys::%exit code))")
before_notimpl(r"'raw-command-line-arguments", "#+clysm (cons \"clysm\" sys::*posix-argv*)")
before_notimpl(r"'chdir\)", "#+clysm (progn (sys::%chdir x) (setf *default-pathname-defaults* (pathname (sys::%getcwd))))")

# getcwd has no feature guard before the error
rep("""        (not-implemented-error 'getcwd)))""",
    """        #+clysm (pathname (sys::%getcwd))
        (not-implemented-error 'getcwd)))""")

# (setf getenv): quoted and backquoted variants, set and unset
s, n1 = re.subn(r"(\n[ \t]*)#\+sbcl `\(progn \(require :sb-posix\) \(symbol-call :sb-posix :setenv ,x ,val 1\)\)\n([ \t]*)#-\(or ",
                r"\1#+sbcl `(progn (require :sb-posix) (symbol-call :sb-posix :setenv ,x ,val 1))\1#+clysm `(sys::%setenv ,x ,val)\n\2#-(or clysm ", s)
s, n2 = re.subn(r"(\n[ \t]*)#\+sbcl `\(progn \(require :sb-posix\) \(symbol-call :sb-posix :unsetenv ,x\)\)\n([ \t]*)#-\(or ",
                r"\1#+sbcl `(progn (require :sb-posix) (symbol-call :sb-posix :unsetenv ,x))\1#+clysm `(sys::%setenv ,x nil)\n\2#-(or clysm ", s)
s, n3 = re.subn(r"(\n[ \t]*)#\+sbcl \(progn \(require :sb-posix\) \(symbol-call :sb-posix :setenv ,x ,val 1\)\)\n([ \t]*)#-\(or ",
                r"\1#+sbcl (progn (require :sb-posix) (symbol-call :sb-posix :setenv ,x ,val 1))\1#+clysm (sys::%setenv ,x ,val)\n\2#-(or clysm ", s)
s, n4 = re.subn(r"(\n[ \t]*)#\+sbcl \(progn \(require :sb-posix\) \(symbol-call :sb-posix :unsetenv ,x\)\)\n([ \t]*)#-\(or ",
                r"\1#+sbcl (progn (require :sb-posix) (symbol-call :sb-posix :unsetenv ,x))\1#+clysm (sys::%setenv ,x nil)\n\2#-(or clysm ", s)
assert (n1, n2, n3, n4) == (1, 1, 1, 1), (n1, n2, n3, n4)

# the fasl cache directory names the image build: compiled modules in fasls
# are only valid for the image that made them
rep("""        #+ecl (format nil "~A~@[-~A~]" s""",
    """        #+clysm (format nil "~A-~A" s (or sys::*image-id* "interpreter"))
        #+ecl (format nil "~A~@[-~A~]" s""")

# package-local nicknames are in SYSTEM
rep("""   #+lispworks #:hcl
   #-(or allegro sbcl clasp abcl ccl lispworks ecl)""",
    """   #+lispworks #:hcl
   #+clysm #:system
   #-(or allegro sbcl clasp abcl ccl lispworks ecl clysm)""")

# implementation type and architecture
rep("(:cmu :cmucl :cmu) :clasp :ecl :gcl", "(:cmu :cmucl :cmu) :clasp :clysm :ecl :gcl")
rep("(:java :java :java-1.4 :java-1.5 :java-1.6 :java-1.7))))",
    "(:java :java :java-1.4 :java-1.5 :java-1.6 :java-1.7)\n       :wasm)))")

# no :unspecific pathname components
s, n = re.subn(r"(\(defparameter \*unspecific-pathname-type\*\n[^\n]*\n[ \t]*#\+\(or )", r"\1clysm ", s, count=1)
assert n == 1

# hook ASDF into REQUIRE
s, n = re.subn(r"#\+\(or abcl clasp cmucl clozure ecl (mezzano )?mkcl sbcl\)\n(  \(:import-from #\+abcl :sys)",
               lambda m: "#+(or abcl clasp clysm cmucl clozure ecl " + (m.group(1) or "") + "mkcl sbcl)\n" + m.group(2) + " #+clysm :system", s, count=1)
assert n == 1
s, n = re.subn(r"(;;;; Hook ASDF into the implementation's REQUIRE and other entry points.\n#\+\(or abcl clasp clisp )",
               r"\1clysm ", s, count=1)
assert n == 1

# subprocesses (runtime/process.lisp)
rep("""  (defun run-program (command &rest keys
                       &key ignore-error-status (force-shell nil force-shell-suppliedp)""",
    """  #+clysm
  (defun %clysm-deliver (text spec if-exists default-stream)
    ;; the output TEXT of a process, as SPEC of RUN-PROGRAM asks
    (cond ((null spec) nil)
          ((eq spec :interactive) nil)
          ((eq spec t) (write-string text default-stream) nil)
          ((or (stringp spec) (pathnamep spec))
           (with-output-to-file (s spec :if-exists if-exists :if-does-not-exist :create)
             (write-string text s))
           nil)
          ((streamp spec) (write-string text spec) nil)
          (t (slurp-input-stream spec (make-string-input-stream text)))))

  #+clysm
  (defun %clysm-run-program (command &key ignore-error-status force-shell
                                       input (if-input-does-not-exist :error)
                                       output (if-output-exists :supersede)
                                       error-output (if-error-output-exists :supersede)
                                       directory environment &allow-other-keys)
    (let* ((command (if (and force-shell (listp command)) (escape-shell-command command) command))
           (input-text (cond ((null input) nil)
                             ((eq input :interactive) :interactive)
                             ((eq input t) (slurp-stream-string *standard-input*))
                             ((or (stringp input) (pathnamep input))
                              (%handle-if-does-not-exist input if-input-does-not-exist)
                              (read-file-string input))
                             ((streamp input) (slurp-stream-string input))
                             (t (parameter-error "~S: ~S is not a valid input for ~S" 'run-program input 'run-program))))
           (merge (eq error-output :output))
           (output-mode (cond ((null output) nil) ((eq output :interactive) :interactive) (t :pipe)))
           (error-mode (cond (merge output-mode) ((null error-output) nil)
                             ((eq error-output :interactive) :interactive) (t :pipe))))
      (multiple-value-bind (out err code)
          (sys::%run-program-sync command :input input-text :directory directory :environment environment
                                          :output output-mode :error-output error-mode)
        (when merge (setf out (concatenate 'string (or out "") (or err "")) err nil))
        (let ((output-result (and out (%clysm-deliver out output if-output-exists *standard-output*)))
              (error-result (and err (not merge)
                                 (%clysm-deliver err error-output if-error-output-exists *error-output*))))
          (unless (or ignore-error-status (eql code 0))
            (cerror "IGNORE-ERROR-STATUS" 'subprocess-error :command command :code code :process nil))
          (values output-result error-result code)))))

  (defun run-program (command &rest keys
                       &key ignore-error-status (force-shell nil force-shell-suppliedp)""")
rep("""                        if-error-output-exists element-type external-format ignore-error-status))
    #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl lispworks mcl mkcl sbcl scl xcl)
    (not-implemented-error 'run-program)""",
    """                        if-error-output-exists element-type external-format ignore-error-status))
    #+clysm (return-from run-program (apply '%clysm-run-program command keys))
    #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl lispworks mcl mkcl sbcl scl xcl)
    (not-implemented-error 'run-program)""")
rep("""  (defun launch-program (command &rest keys""",
    """  #+clysm
  (defun %clysm-launch-program (command &key input (if-input-does-not-exist :error)
                                          output (if-output-exists :supersede)
                                          error-output (if-error-output-exists :supersede)
                                          directory environment (element-type 'character)
                                        &allow-other-keys)
    ;; (T, this Lisp's standard streams, is the process's: :INTERACTIVE)
    (flet ((spec (x) (if (eq x t) :interactive x)))
      (%handle-if-does-not-exist input if-input-does-not-exist)
      (%handle-if-exists output if-output-exists)
      (unless (eq error-output :output) (%handle-if-exists error-output if-error-output-exists))
      (multiple-value-bind (process in out err)
          (sys::%launch-program command :input (spec input) :output (spec output)
                                        :error-output (spec error-output)
                                        :if-output-exists if-output-exists
                                        :if-error-output-exists if-error-output-exists
                                        :directory directory :environment environment
                                        :element-type element-type)
        (let ((info (make-instance 'process-info)))
          (setf (slot-value info 'process) process
                (slot-value info 'input-stream) in
                (slot-value info 'output-stream) out
                (slot-value info 'error-output-stream) err)
          info))))

  (defun launch-program (command &rest keys""")
rep("""could block because its output buffers are full."
    #-(or abcl allegro clasp clozure cmucl ecl (and lispworks os-unix) mkcl sbcl scl)""",
    """could block because its output buffers are full."
    #+clysm (return-from launch-program (apply '%clysm-launch-program command keys))
    #-(or abcl allegro clasp clozure cmucl ecl (and lispworks os-unix) mkcl sbcl scl)""")
before_notimpl(r"'process-info-pid\)", "#+clysm (sys::%process-pid process)")
before_notimpl(r"'%process-status\)",
               "#+clysm (return-from %process-status (let ((code (sys::%process-status (slot-value process-info 'process)))) (if code (values :exited (setf (slot-value process-info 'exit-code) code)) :running)))")
before_notimpl(r"'wait-process\)",
               "#+clysm (return-from wait-process (setf (slot-value process-info 'exit-code) (sys::%process-wait (slot-value process-info 'process))))")
rep("""    (declare (ignorable urgent))
    #+abcl (sys:process-kill (slot-value process-info 'process))""",
    """    (declare (ignorable urgent))
    #+clysm (return-from terminate-process (sys::%process-kill (slot-value process-info 'process) urgent))
    #+abcl (sys:process-kill (slot-value process-info 'process))""")

open(p, 'w').write(s)
print("ported")
