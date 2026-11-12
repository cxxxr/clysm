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

open(p, 'w').write(s)
print("ported")
