;;;; pathname.lisp — pathnames (CLHS 19)
;;;;
;;;; Physical pathnames use Unix namestrings.  Logical pathnames
;;;; ("HOST:DIR;DIR;NAME.TYPE.VERSION") are supported for hosts defined with
;;;; (SETF LOGICAL-PATHNAME-TRANSLATIONS).  File system access goes through
;;;; the host's fs module when it is available (Node.js).

(defstruct (pathname (:constructor %make-pathname (host device directory name type version))
                     (:predicate pathnamep)
                     (:conc-name %pathname-)
                     (:copier nil))
  host device directory name type version)

(defun pathname-host (p &key case) (declare (ignore case)) (%pathname-host (pathname p)))
(defun pathname-device (p &key case) (declare (ignore case)) (%pathname-device (pathname p)))
(defun pathname-directory (p &key case) (declare (ignore case)) (%pathname-directory (pathname p)))
(defun pathname-name (p &key case) (declare (ignore case)) (%pathname-name (pathname p)))
(defun pathname-type (p &key case) (declare (ignore case)) (%pathname-type (pathname p)))
(defun pathname-version (p) (%pathname-version (pathname p)))

(defstruct (logical-pathname (:include pathname)
                             (:constructor %make-logical-pathname (host device directory name type version))
                             (:copier nil)))

(defvar *logical-pathname-translations* (make-hash-table :test 'equalp))

;; universal time of 1970-01-01 (FILE-WRITE-DATE here, GET-UNIVERSAL-TIME in misc.lisp)
(defconstant +unix-epoch-offset+ 2208988800)

(defun %host-fs ()
  (ignore-errors
   (let ((fs (js:global "__clysm_fs")))
     (and (js:js-value-p fs) fs))))

;;; ------------------------------------------------------------
;;; parsing
;;; ------------------------------------------------------------

(defun %wild-word (s)
  (cond ((string= s "*") :wild)
        ((string= s "**") :wild-inferiors)
        (t s)))

(defun %split-string (s ch)
  (let ((parts nil) (start 0))
    (dotimes (i (length s))
      (when (char= (char s i) ch)
        (push (subseq s start i) parts)
        (setq start (+ i 1))))
    (push (subseq s start) parts)
    (nreverse parts)))

(defun %windows-device (s)
  "On a Windows host, the device at the start of the namestring S (whose
backslashes are slashes) and the rest of S; otherwise NIL and S."
  (cond ((not *windows-paths*) (values nil s))
        ((and (>= (length s) 2) (char= (char s 1) #\:) (alpha-char-p (char s 0)))
         (values (string (char-upcase (char s 0))) (subseq s 2)))
        ((and (> (length s) 2) (char= (char s 0) #\/) (char= (char s 1) #\/)
              (not (char= (char s 2) #\/)))
         ;; //server/share/...: the device is "//server/share"
         (let* ((server-end (position #\/ s :start 2))
                (share-end (and server-end (position #\/ s :start (+ server-end 1)))))
           (if server-end
               (values (subseq s 0 (or share-end (length s))) (if share-end (subseq s share-end) "/"))
               (values nil s))))
        (t (values nil s))))

(defun %device-prefix (device)
  (cond ((not (stringp device)) "")
        ((and (> (length device) 1) (char= (char device 0) #\/)) device)
        (t (concatenate 'string device ":"))))

(defun %parse-physical-namestring (s)
  (multiple-value-bind (device s)
      (%windows-device (if *windows-paths* (substitute #\/ #\\ s) s))
    (let ((p (%parse-physical-namestring-1 s)))
      (if device
          (%make-pathname nil device (pathname-directory p) (pathname-name p) (pathname-type p) nil)
          p))))

(defun %parse-physical-namestring-1 (s)
  (let* ((slash (position #\/ s :from-end t))
         (dir-part (if slash (subseq s 0 (+ slash 1)) nil))
         (file (if slash (subseq s (+ slash 1)) s))
         (dot (position #\. file :from-end t))
         (name (cond ((string= file "") nil)
                     ((and dot (> dot 0)) (subseq file 0 dot))
                     (t file)))
         (type (if (and dot (> dot 0)) (subseq file (+ dot 1)) nil))
         (directory
           (when dir-part
             (let* ((absolute (char= (char dir-part 0) #\/))
                    (parts (remove "" (%split-string dir-part #\/) :test #'string=)))
               (cons (if absolute :absolute :relative)
                     (mapcar (lambda (d) (cond ((string= d "..") :up) (t (%wild-word d)))) parts))))))
    (%make-pathname nil nil directory (and name (%wild-word name)) (and type (%wild-word type)) nil)))

(defun %logical-host-p (host) (nth-value 1 (gethash host *logical-pathname-translations*)))

(defun %parse-logical-namestring (s host-end)
  (let* ((host (string-upcase (subseq s 0 host-end)))
         (rest (subseq s (+ host-end 1)))
         (semi (position #\; rest :from-end t))
         (dir-part (if semi (subseq rest 0 (+ semi 1)) nil))
         (file (if semi (subseq rest (+ semi 1)) rest))
         (fields (%split-string file #\.))
         (relative (and dir-part (> (length dir-part) 0) (char= (char dir-part 0) #\;)))
         (dirs (and dir-part (remove "" (%split-string dir-part #\;) :test #'string=))))
    (flet ((comp (x) (if (or (null x) (string= x "")) nil (%wild-word (string-upcase x)))))
      (%make-logical-pathname
       host :unspecific
       (cons (if relative :relative :absolute)
             (mapcar (lambda (d) (if (string= d "..") :up (comp d))) dirs))
       (comp (first fields)) (comp (second fields))
       (let ((v (third fields)))
         (cond ((null v) nil)
               ((string-equal v "NEWEST") :newest)
               ((string= v "*") :wild)
               (t (parse-integer v :junk-allowed t))))))))

(defun %parse-namestring (s)
  (let ((colon (position #\: s)))
    (if (and colon (> colon 0) (%logical-host-p (string-upcase (subseq s 0 colon))))
        (%parse-logical-namestring s colon)
        (%parse-physical-namestring s))))

(defun pathname (x)
  (cond ((pathnamep x) x)
        ((stringp x) (%parse-namestring x))
        ((%streamp x) (or (stream-pathname x) (error 'type-error :datum x :expected-type 'file-stream)))
        (t (%type-error x '(or pathname string stream)))))

(defun pathname-p* (x) (pathnamep x))

(defun parse-namestring (thing &optional host (defaults *default-pathname-defaults*)
                               &key (start 0) end junk-allowed)
  (declare (ignore defaults junk-allowed))
  (cond ((pathnamep thing) (values thing start))
        ((%streamp thing) (values (pathname thing) start))
        (t (let* ((s (string thing)) (end (or end (length s)))
                  (sub (subseq s start end)))
             (values (if (and host (%logical-host-p (string-upcase (string host))) (not (position #\: sub)))
                         (%parse-logical-namestring (concatenate 'string (string host) ":" sub) (length (string host)))
                         (%parse-namestring sub))
                     end)))))

;;; ------------------------------------------------------------
;;; components
;;; ------------------------------------------------------------


(defun %valid-directory-p (dir)
  (or (member dir '(nil :wild :unspecific))
      (and (consp dir) (member (car dir) '(:absolute :relative)))))

(defun make-pathname (&key (host nil hostp) (device nil devicep) (directory nil dirp) (name nil namep)
                           (type nil typep) (version nil versionp) defaults (case :local))
  (declare (ignore case))
  (let* ((d (if defaults (pathname defaults)
                (%make-pathname (and hostp host) nil nil nil nil nil)))
         (host (if hostp host (pathname-host d)))
         (directory (cond ((not dirp) (pathname-directory d))
                          ((stringp directory) (list :absolute directory))
                          ((eq directory :wild) (list :absolute :wild-inferiors))
                          (t directory))))
    (unless (%valid-directory-p directory)
      (error 'type-error :datum directory :expected-type 'list))
    (funcall (if (and (stringp host) (%logical-host-p (string-upcase host)))
                 #'%make-logical-pathname #'%make-pathname)
             host
             (if devicep device (pathname-device d))
             directory
             (if namep name (pathname-name d))
             (if typep type (pathname-type d))
             (if versionp version (pathname-version d)))))

;;; ------------------------------------------------------------
;;; namestrings
;;; ------------------------------------------------------------

(defun %component-string (c)
  (case c (:wild "*") (:wild-inferiors "**") (:up "..") (:back "..") (t c)))

(defun %logical-namestring (p)
  (with-output-to-string (s)
    (write-string (pathname-host p) s) (write-char #\: s)
    (let ((dir (pathname-directory p)))
      (when dir
        (when (eq (car dir) :relative) (write-char #\; s))
        (dolist (d (cdr dir)) (write-string (%component-string d) s) (write-char #\; s))))
    (when (pathname-name p) (write-string (%component-string (pathname-name p)) s))
    (when (pathname-type p) (write-char #\. s) (write-string (%component-string (pathname-type p)) s))
    (let ((v (pathname-version p)))
      (when (and v (not (eq v :unspecific)))
        (write-char #\. s)
        (write-string (case v (:newest "NEWEST") (:wild "*") (t (princ-to-string v))) s)))))

(defun namestring (x)
  (let ((p (pathname x)))
    (if (logical-pathname-p p)
        (%logical-namestring p)
        (concatenate 'string (%device-prefix (pathname-device p))
                     (directory-namestring p) (file-namestring p)))))

(defun file-namestring (x)
  (let* ((p (pathname x)) (name (pathname-name p)) (type (pathname-type p)))
    (concatenate 'string
                 (if (stringp (%component-string name)) (%component-string name) "")
                 (if (and type (not (eq type :unspecific))) "." "")
                 (if (stringp (%component-string type)) (%component-string type) ""))))

(defun directory-namestring (x)
  (let ((dir (pathname-directory (pathname x))))
    (with-output-to-string (s)
      (when (consp dir)
        (when (eq (car dir) :absolute) (write-char #\/ s))
        (dolist (d (cdr dir))
          (write-string (%component-string d) s) (write-char #\/ s))))))

(defun host-namestring (x)
  (let ((h (pathname-host (pathname x)))) (if (stringp h) h "")))

(defun enough-namestring (x &optional (defaults *default-pathname-defaults*))
  (let ((p (pathname x)) (d (pathname defaults)))
    (let ((pd (pathname-directory p)) (dd (pathname-directory d)))
      (if (and (consp pd) (consp dd) (eq (car pd) :absolute) (eq (car dd) :absolute)
               (<= (length dd) (length pd)) (equal dd (subseq pd 0 (length dd))))
          (namestring (make-pathname :directory (cons :relative (nthcdr (length dd) pd)) :defaults p))
          (namestring p)))))

;;; ------------------------------------------------------------
;;; merging, wildcards, translation
;;; ------------------------------------------------------------

(defvar *default-pathname-defaults* (%make-pathname nil nil nil nil nil nil))

(defun merge-pathnames (path &optional (defaults *default-pathname-defaults*) (default-version :newest))
  (let* ((d (pathname defaults))
         (p (if (and (stringp path) (logical-pathname-p d) (not (position #\: path)))
                (parse-namestring path (pathname-host d))
                (pathname path)))
         (pd (pathname-directory p))
         (dir (cond ((null pd) (pathname-directory d))
                    ((and (eq (car pd) :relative) (consp (pathname-directory d)))
                     (let ((merged (append (pathname-directory d) (cdr pd))))
                       ;; remove name/.. pairs
                       (let ((r nil))
                         (dolist (c merged (nreverse r))
                           (if (and (eq c :back) r (stringp (car r))) (pop r) (push c r))))))
                    (t pd))))
    (funcall (if (logical-pathname-p (if (pathname-host p) p d)) #'%make-logical-pathname #'%make-pathname)
             (or (pathname-host p) (pathname-host d))
             (or (pathname-device p) (pathname-device d))
             dir
             (or (pathname-name p) (pathname-name d))
             (or (pathname-type p) (pathname-type d))
             (or (pathname-version p)
                 (if (pathname-name p) default-version (or (pathname-version d) default-version))))))

(defun wild-pathname-p (p &optional field)
  (let ((p (pathname p)))
    (flet ((wild-dir () (and (consp (pathname-directory p))
                             (some (lambda (c) (or (member c '(:wild :wild-inferiors))
                                                   (and (stringp c) (find #\* c))))
                                   (cdr (pathname-directory p)))))
           (wild (c) (or (eq c :wild) (and (stringp c) (find #\* c)))))
      (case field
        ((nil) (not (null (or (wild (pathname-host p)) (wild (pathname-device p)) (wild-dir)
                   (eq (pathname-directory p) :wild)
                   (wild (pathname-name p)) (wild (pathname-type p)) (wild (pathname-version p))))))
        (:host (not (null (wild (pathname-host p)))))
        (:device (not (null (wild (pathname-device p)))))
        (:directory (not (null (wild-dir))))
        (:name (not (null (wild (pathname-name p)))))
        (:type (not (null (wild (pathname-type p)))))
        (:version (not (null (wild (pathname-version p)))))
        (t (error 'type-error :datum field :expected-type '(member nil :host :device :directory :name :type :version)))))))

(defun %glob-match (pattern string)
  (cond ((eq pattern :wild) t)
        ((null pattern) (null string))
        ((not (stringp pattern)) (equal pattern string))
        ((not (stringp string)) nil)
        (t (labels ((m (i j)
                      (cond ((= i (length pattern)) (= j (length string)))
                            ((char= (char pattern i) #\*)
                             (loop for k from j to (length string) thereis (m (+ i 1) k)))
                            ((= j (length string)) nil)
                            ((char-equal (char pattern i) (char string j)) (m (+ i 1) (+ j 1)))
                            (t nil))))
             (m 0 0)))))

(defun %dir-match (pattern dir)
  (cond ((or (null pattern) (eq pattern :wild)) t)
        ((null dir) (null pattern))
        (t (labels ((m (ps ds)
                      (cond ((null ps) (null ds))
                            ((eq (car ps) :wild-inferiors)
                             (loop for tail on (cons nil ds) thereis (m (cdr ps) (cdr tail))))
                            ((null ds) nil)
                            ((%glob-match (car ps) (car ds)) (m (cdr ps) (cdr ds)))
                            (t nil))))
             (and (eq (car pattern) (car dir)) (m (cdr pattern) (cdr dir)))))))

(defun pathname-match-p (path wildcard)
  (let ((p (pathname path)) (w (pathname wildcard)))
    (and (%dir-match (pathname-directory w) (pathname-directory p))
         (or (null (pathname-name w)) (%glob-match (pathname-name w) (pathname-name p)))
         (or (null (pathname-type w)) (%glob-match (pathname-type w) (pathname-type p)))
         (or (member (pathname-version w) '(nil :wild :newest)) (eql (pathname-version w) (pathname-version p)))
         t)))

(defun %translate-component (source from to)
  (cond ((or (null to)) source)
        ((eq to :wild) source)
        ((and (stringp to) (find #\* to) (stringp source))
         ;; replace the * by the part of SOURCE matched by * in FROM
         (let ((star (position #\* to))
               (matched (if (and (stringp from) (find #\* from))
                            (let* ((fs (position #\* from)) (prefix (subseq from 0 fs))
                                   (suffix (subseq from (+ fs 1))))
                              (subseq source (length prefix) (- (length source) (length suffix))))
                            source)))
           (concatenate 'string (subseq to 0 star) matched (subseq to (+ star 1)))))
        (t to)))

(defun %translate-directory (source from to)
  (cond ((null to) source)
        ((not (consp to)) source)
        (t (let ((wild-part (if (consp from)
                                (let ((n (position :wild-inferiors (cdr from))))
                                  (if n (nthcdr n (cdr source)) nil))
                                (cdr source))))
             (append (list (car to))
                     (loop for c in (cdr to)
                           append (if (eq c :wild-inferiors) wild-part (list c))))))))

(defun translate-pathname (source from-wildcard to-wildcard &key)
  (let ((s (pathname source)) (f (pathname from-wildcard)) (to (pathname to-wildcard)))
    (funcall (if (logical-pathname-p to) #'%make-logical-pathname #'%make-pathname)
             (pathname-host to) (pathname-device to)
             (%translate-directory (pathname-directory s) (pathname-directory f) (pathname-directory to))
             (%translate-component (pathname-name s) (pathname-name f) (pathname-name to))
             (%translate-component (pathname-type s) (pathname-type f) (pathname-type to))
             (if (member (pathname-version to) '(nil :wild)) (pathname-version s) (pathname-version to)))))

(defun logical-pathname-translations (host)
  (multiple-value-bind (v found) (gethash (string host) *logical-pathname-translations*)
    (if found v (error 'type-error :datum host :expected-type 'logical-host))))

(defun %set-lpt (host translations)
  (setf (gethash (string host) *logical-pathname-translations*) nil)
  (setf (gethash (string host) *logical-pathname-translations*)
        (mapcar (lambda (e) (list (pathname (if (stringp (car e))
                                                (if (position #\: (car e)) (car e)
                                                    (concatenate 'string (string host) ":" (car e)))
                                                (car e)))
                                  (pathname (cadr e))))
                translations))
  translations)
(defun (setf logical-pathname-translations) (translations host) (%set-lpt host translations))

(defun load-logical-pathname-translations (host)
  (if (%logical-host-p (string-upcase (string host))) nil
      (error 'file-error :pathname host :format-control "no translations for logical host ~A"
                         :format-arguments (list host))))

(defun logical-pathname (x)
  (let ((p (if (stringp x)
               (let ((colon (position #\: x)))
                 (unless (and colon (%logical-host-p (string-upcase (subseq x 0 colon))))
                   (error 'type-error :datum x :expected-type 'logical-pathname))
                 (%parse-logical-namestring x colon))
               (pathname x))))
    (unless (logical-pathname-p p) (error 'type-error :datum x :expected-type 'logical-pathname))
    p))

(defun translate-logical-pathname (p &key)
  (let ((p (pathname p)))
    (if (not (logical-pathname-p p))
        p
        (let ((translations (logical-pathname-translations (pathname-host p))))
          (dolist (e translations (error 'file-error :pathname p :format-control "no translation for ~A"
                                                     :format-arguments (list (namestring p))))
            (when (pathname-match-p p (car e))
              (let ((r (translate-pathname p (car e) (cadr e))))
                (return (translate-logical-pathname
                         (if (logical-pathname-p r) r (%downcase-pathname r)))))))))))

(defun %downcase-pathname (p)
  (flet ((d (c) (if (stringp c) (string-downcase c) c)))
    (%make-pathname (pathname-host p) (pathname-device p)
                    (let ((dir (pathname-directory p))) (if (consp dir) (mapcar #'d dir) dir))
                    (d (pathname-name p)) (d (pathname-type p)) (pathname-version p))))

(defun %physical (x) (translate-logical-pathname (merge-pathnames x)))

;;; ------------------------------------------------------------
;;; file system
;;; ------------------------------------------------------------

(defun %native-name (x) (namestring (%physical x)))

(defun %file-exists-p (native)
  (let ((fs (%host-fs)))
    (if fs (js:call fs "existsSync" native) (not (null (%host-read-octets native))))))

(defun %host-rename-file (old new)
  (let ((fs (%host-fs)))
    (when fs (js:call fs "renameSync" old new))))

(defun %fs-pathname (x)
  "The pathname designated by X (a file stream designates its pathname); a
FILE-ERROR for wild pathnames."
  (let ((p (pathname x)))
    (when (wild-pathname-p p)
      (error 'file-error :pathname p :format-control "wild pathname ~A not allowed here"
                         :format-arguments (list (namestring p))))
    p))

(defun %host-directory-p (fs native)
  (ignore-errors (js:call (js:call fs "statSync" native) "isDirectory")))

(defun %truename-or-nil (x)
  (let* ((p (%fs-pathname x)) (fs (%host-fs)) (name (%native-name p)))
    (if fs
        (and (js:call fs "existsSync" name)
             (let ((real (or (ignore-errors (js:call fs "realpathSync" name)) name)))
               (if (and (%host-directory-p fs real)
                        (not (and (> (length real) 0) (char= (char real (- (length real) 1)) #\/))))
                   (pathname (concatenate 'string real "/"))
                   (pathname real))))
        (and (%host-read-octets name) (pathname name)))))

(defun probe-file (x) (%truename-or-nil x))

(defun truename (x)
  (or (%truename-or-nil x)
      (error 'file-error :pathname x :format-control "the file ~A does not exist"
                         :format-arguments (list (namestring x)))))

(defun delete-file (x)
  (let* ((p (%fs-pathname x)) (fs (%host-fs)) (name (%native-name p)))
    (unless (and fs (js:call fs "existsSync" name))
      (error 'file-error :pathname x :format-control "cannot delete ~A: no such file"
                         :format-arguments (list name)))
    (handler-case
        (if (%host-directory-p fs name)
            (js:call fs "rmdirSync" name)
            (js:call fs "unlinkSync" name))
      (error (c)
        (error 'file-error :pathname x :format-control "cannot delete ~A: ~A"
                           :format-arguments (list name c))))
    (when (%streamp x) (setf (stream-open x) nil (stream-fast x) nil (stream-fast-out x) nil))
    t))

(defun rename-file (from to)
  (let* ((fs (%host-fs))
         (from-path (%fs-pathname from))
         (old-true (truename from))
         (old (%native-name from-path))
         (new-path (merge-pathnames to from-path))
         (new (%native-name new-path)))
    (when (wild-pathname-p new-path)
      (error 'file-error :pathname to :format-control "wild pathname ~A not allowed here"
                         :format-arguments (list (namestring new-path))))
    (when fs
      (handler-case (js:call fs "renameSync" old new)
        (error (c)
          (error 'file-error :pathname from :format-control "cannot rename ~A: ~A"
                             :format-arguments (list old c)))))
    (values new-path old-true (truename new-path))))

(defun ensure-directories-exist (p &key verbose)
  (declare (ignore verbose))
  (let* ((path (pathname p)))
    (when (wild-pathname-p path :directory)
      (error 'file-error :pathname path :format-control "wild directory in ~A"
                         :format-arguments (list (namestring path))))
    (let* ((fs (%host-fs))
           (dir (let ((p (%physical path)))
                  (concatenate 'string (%device-prefix (pathname-device p)) (directory-namestring p))))
           (existed (or (null fs) (equal dir "") (js:call fs "existsSync" dir))))
      (when (and fs (not existed))
        (js:call fs "mkdirSync" dir (js:object "recursive" t)))
      (values p (not existed)))))

(defun file-write-date (p)
  (let* ((path (%fs-pathname p)) (fs (%host-fs)) (name (%native-name path)))
    (if (and fs (js:call fs "existsSync" name))
        (+ +unix-epoch-offset+ (floor (js:get (js:call fs "statSync" name) "mtimeMs") 1000))
        nil)))

(defun file-author (p)
  (%fs-pathname p)
  nil)

(defun %check-directory-components (p)
  "A FILE-ERROR for directories that cannot name anything: (:absolute :up ...),
(:absolute :back ...) and :wild-inferiors followed by :up or :back."
  (let ((dir (pathname-directory p)))
    (when (and (consp dir)
               (or (and (eq (car dir) :absolute) (member (cadr dir) '(:up :back)))
                   (loop for (a b) on (cdr dir)
                         thereis (and (eq a :wild-inferiors) (member b '(:up :back))))))
      (error 'file-error :pathname p :format-control "invalid directory ~S"
                         :format-arguments (list dir)))))

(defun %dir-entries (fs dir)
  "The entries of the host directory DIR (a string ending in / or empty), as
a list of (name . directory-p)."
  (let ((entries (ignore-errors (js:call fs "readdirSync" (if (string= dir "") "." dir)))))
    (when entries
      (let ((r nil))
        (dotimes (i (js:get entries "length") (nreverse r))
          (let ((name (js:get entries i)))
            (push (cons name (not (null (%host-directory-p fs (concatenate 'string dir name)))))
                  r)))))))

(defun %expand-wild-directories (fs comps base)
  "The host directories (strings ending in /) below BASE matching the
directory components COMPS, which may contain :WILD, :WILD-INFERIORS, wild
strings, :UP and :BACK."
  (if (null comps)
      (list base)
      (let ((c (car comps)) (rest (cdr comps)))
        (cond ((member c '(:up :back))
               (%expand-wild-directories fs rest (concatenate 'string base "../")))
              ((eq c :wild-inferiors)
               (append (%expand-wild-directories fs rest base)
                       (loop for (name . dirp) in (%dir-entries fs base)
                             when dirp
                               append (%expand-wild-directories
                                       fs comps (concatenate 'string base name "/")))))
              ((or (eq c :wild) (and (stringp c) (find #\* c)))
               (loop for (name . dirp) in (%dir-entries fs base)
                     when (and dirp (%glob-match c name))
                       append (%expand-wild-directories fs rest (concatenate 'string base name "/"))))
              (t (let ((sub (concatenate 'string base (%component-string c) "/")))
                   (and (%host-directory-p fs sub)
                        (%expand-wild-directories fs rest sub))))))))

(defun directory (p &key)
  (%check-directory-components (pathname p))
  (let* ((fs (%host-fs)) (pat (%physical (merge-pathnames p)))
         (dirspec (pathname-directory pat)))
    (if (null fs) nil
        (let* ((dirs (if (consp dirspec)
                         (%expand-wild-directories fs (cdr dirspec)
                                                   (if (eq (car dirspec) :absolute)
                                                       (concatenate 'string (%device-prefix (pathname-device pat)) "/")
                                                       ""))
                         (list "")))
               (results nil))
          (dolist (dir dirs)
            (if (and (null (pathname-name pat)) (null (pathname-type pat)))
                ;; a directory pattern: the matching directories themselves
                (push (pathname dir) results)
                (dolist (e (%dir-entries fs dir))
                  (let ((full (pathname (concatenate 'string dir (car e)))))
                    (when (and (or (null (pathname-name pat))
                                   (%glob-match (pathname-name pat) (pathname-name full)))
                               (or (null (pathname-type pat))
                                   (%glob-match (pathname-type pat) (pathname-type full))))
                      (push full results))))))
          (let ((seen (make-hash-table :test 'equal)) (out nil))
            (dolist (r results)
              (let* ((tn (or (ignore-errors (truename r)) r)) (key (namestring tn)))
                (unless (gethash key seen)
                  (setf (gethash key seen) t)
                  (push tn out))))
            (sort out #'string< :key #'namestring))))))

(defun user-homedir-pathname (&optional host)
  (declare (ignore host))
  (let ((home (or (%getenv "HOME") (%getenv "USERPROFILE"))))
    (pathname (if (and home (> (length home) 0))
                  (if (char= (char home (1- (length home))) #\/) home (concatenate 'string home "/"))
                  "/"))))
(defun %pathname-components (p)
  (list (pathname-host p) (pathname-device p) (pathname-directory p)
        (pathname-name p) (pathname-type p) (pathname-version p)))

(defmethod print-object ((p pathname) stream)
  (cond ((and *print-readably*
              (not (ignore-errors (equal (%pathname-components p)
                                         (%pathname-components (pathname (namestring p)))))))
         ;; the namestring does not read back as P (e.g. its version)
         (format stream "#.(~S ~S ~S ~S ~S ~S '~S ~S ~S ~S ~S ~S ~S)"
                 'make-pathname :host (pathname-host p) :device (pathname-device p)
                 :directory (pathname-directory p) :name (pathname-name p)
                 :type (pathname-type p) :version (pathname-version p)))
        ((or *print-escape* *print-readably*)
         (write-string "#P" stream) (prin1 (namestring p) stream))
        (t (write-string (namestring p) stream)))
  p)
