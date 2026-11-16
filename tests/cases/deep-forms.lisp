;;; A function whose forms nest deep (a loop in a loop in a LET, several
;;; times over) with calls of a function inside: what a macro that compiles
;;; a pattern into loops expands to.  The definitions of a program are also
;;; evaluated by the compiler's host for the program's macros to call, and
;;; SBCL's own compiler did not finish with this one (minutes; the time went
;;; to its propagation of constraints).  This file has its expected output
;;; written out (deep-forms.expected): SBCL is not asked to run it.
(defun space-p (char)
  (let ((code (char-code char)))
    (or (= code 32) (<= 9 code 13)
        (and (>= code #xA0)
             (or (= code #xA0) (= code #x1680) (<= #x2000 code #x200A) (= code #x2028) (= code #x2029)
                 (= code #x202F) (= code #x205F) (= code #x3000) (= code #xFEFF))))))

(defun f (line)

(let* ((given117 line)
       (string115
        (if (simple-string-p given117)
            given117
            (coerce given117 'simple-string)))
       (end116 (length string115))
       (start118 0)
       (from137 nil)
       (to138 nil)
       (from141 nil)
       (to142 nil)
       (from145 nil)
       (to146 nil)
       (from149 nil)
       (to150 nil))
  (declare (type simple-string string115)
           (type fixnum end116 start118)
           (ignorable from137 to138 from141 to142 from145 to146 from149 to150))
  (if (let ((at119 start118))
        (declare (type fixnum at119))
        (let ((old151 from149) (old152 to150))
          (setq from149 at119
                to150 at119)
          (let ((g153
                 (if (if (<= (+ at119 2) end116)
                         (if (char= (schar string115 (+ at119 0)) #\!)
                             (char= (schar string115 (+ at119 1)) #\[)))
                     (let ((at120 (+ at119 2)))
                       (declare (type fixnum at120))
                       (let ((to121 at120) (times123 0))
                         (declare (type fixnum to121 times123))
                         (block nil
                           (tagbody
                            g154
                             (progn
                              (let ((next122
                                     (if (if (< to121 end116)
                                             (let ((c (schar string115 to121)))
                                               (declare (ignorable c))
                                               (not (the t (char= c #\])))))
                                         (let ((at124 (+ to121 1)))
                                           (declare (type fixnum at124))
                                           at124))))
                                (if (if next122
                                        (> (the fixnum next122) to121))
                                    (progn (setq to121 next122) (setq times123 (+ times123 1)))
                                    (return-from nil nil))))
                             (go g154)))
                         (if (>= times123 0)
                             (let ((old147 from145) (old148 to146))
                               (setq from145 at120
                                     to146 to121)
                               (let ((g155
                                      (if (if (<= (+ to121 2) end116)
                                              (if (char= (schar string115 (+ to121 0)) #\])
                                                  (char= (schar string115 (+ to121 1)) #\()))
                                          (let ((at125 (+ to121 2)))
                                            (declare (type fixnum at125))
                                            (let ((to126 at125) (limit128 (+ at125 0)))
                                              (declare (type fixnum to126 limit128))
                                              (block nil
                                                (tagbody
                                                 next-loop
                                                  (if (if (< to126 end116)
                                                          (let ((c (schar string115 to126)))
                                                            (declare (ignorable c))
                                                            (space-p c)))
                                                      nil
                                                      (go end-loop))
                                                  (setq to126 (+ 1 to126))
                                                  (go next-loop)
                                                 end-loop))
                                              (block nil
                                                (tagbody
                                                 g156
                                                  (progn
                                                   (if (< to126 limit128)
                                                       (return-from nil nil))
                                                   (let ((matched
                                                          (let ((to129 to126) (times131 0))
                                                            (declare (type fixnum to129 times131))
                                                            (block nil
                                                              (tagbody
                                                               g157
                                                                (progn
                                                                 (let ((next130
                                                                        (if (if (< to129 end116)
                                                                                (let ((c (schar string115 to129)))
                                                                                  (declare (ignorable c))
                                                                                  (not (the t (space-p c)))))
                                                                            (let ((at132 (+ to129 1)))
                                                                              (declare (type fixnum at132))
                                                                              at132))))
                                                                   (if (if next130
                                                                           (> (the fixnum next130) to129))
                                                                       (progn
                                                                        (setq to129 next130)
                                                                        (setq times131 (+ times131 1)))
                                                                       (return-from nil nil))))
                                                                (go g157)))
                                                            (if (>= times131 1)
                                                                (let ((old143 from141) (old144 to142))
                                                                  (setq from141 to126
                                                                        to142 to129)
                                                                  (let ((g158
                                                                         (let ((to133 to129) (limit135 (+ to129 0)))
                                                                           (declare (type fixnum to133 limit135))
                                                                           (block nil
                                                                             (tagbody
                                                                              next-loop
                                                                               (if (if (< to133 end116)
                                                                                       (let ((c
                                                                                              (schar string115 to133)))
                                                                                         (declare (ignorable c))
                                                                                         (space-p c)))
                                                                                   nil
                                                                                   (go end-loop))
                                                                               (setq to133 (+ 1 to133))
                                                                               (go next-loop)
                                                                              end-loop))
                                                                           (block nil
                                                                             (tagbody
                                                                              g159
                                                                               (progn
                                                                                (if (< to133 limit135)
                                                                                    (return-from nil nil))
                                                                                (let ((matched
                                                                                       (if (if (<= (+ to133 1) end116)
                                                                                               (char=
                                                                                                (schar string115
                                                                                                       (+ to133 0))
                                                                                                #\)))
                                                                                           (let ((at136 (+ to133 1)))
                                                                                             (declare
                                                                                              (type fixnum at136))
                                                                                             (let ((old139 from137)
                                                                                                   (old140 to138))
                                                                                               (setq from137 at136
                                                                                                     to138 at136)
                                                                                               (let ((g160 t))
                                                                                                 (if g160
                                                                                                     g160
                                                                                                     (progn
                                                                                                      (setq from137
                                                                                                              old139
                                                                                                            to138
                                                                                                              old140)
                                                                                                      nil))))))))
                                                                                  (if matched
                                                                                      (return-from nil matched)))
                                                                                (setq to133
                                                                                        (- to133 1)))
                                                                               (go g159))))))
                                                                    (if g158
                                                                        g158
                                                                        (progn
                                                                         (setq from141 old143
                                                                               to142 old144)
                                                                         nil))))))))
                                                     (if matched
                                                         (return-from nil matched)))
                                                   (setq to126 (- to126 1)))
                                                  (go g156))))))))
                                 (if g155
                                     g155
                                     (progn
                                      (setq from145 old147
                                            to146 old148)
                                      nil))))))))))
            (if g153
                g153
                (progn
                 (setq from149 old151
                       to150 old152)
                 nil)))))
      (let ((end from137)
            (url
             (if from141
                 (subseq string115 from141 to142)))
            (alt
             (if from145
                 (subseq string115 from145 to146)))
            (start from149))
        (declare
         (ignorable end url alt start))
        (progn
         (list start end alt url)))
      nil))
)


(print (f "![alt](  url  )"))
(print (f "![alt text](https://example.com/a.png) and more"))
(print (f "![]( x )"))
(print (f "![x]( )"))
(print (f "nothing"))
