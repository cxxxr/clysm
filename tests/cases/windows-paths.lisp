;;; Namestrings of a Windows host (simulated: *WINDOWS-PATHS* is true there).
(setq sys::*windows-paths* t)
(dolist (s '("C:\\Users\\me\\src\\map.lisp" "c:/work/" "D:relative\\x.txt" "\\\\wsl$\\Ubuntu\\home\\me\\repo\\"
             "//server/share/a/b.c" "/no/device.txt" "plain.txt"))
  (let ((p (pathname s)))
    (print (list s (pathname-device p) (pathname-directory p) (pathname-name p) (pathname-type p)
                 (namestring p)))))
(print (namestring (merge-pathnames "lib/x.lisp" (pathname "C:\\Users\\me\\"))))
(print (namestring (merge-pathnames "/etc/hosts" (pathname "C:\\Users\\me\\"))))
(print (namestring (merge-pathnames "y.txt" (pathname "\\\\wsl$\\Ubuntu\\home\\"))))
(setq sys::*windows-paths* nil)
(print (list (pathname-device "C:/x") (namestring "C:/x")))
(terpri)
