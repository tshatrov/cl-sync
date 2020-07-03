(in-package :cl-user)

(defpackage :cl-sync
  (:use :cl :cl-fad :cl-ppcre)
  (:nicknames :sync))

(in-package :cl-sync)

;; defining wstat, which works with unicode filenames
(sb-posix::define-stat-call "_wstat"
                  sb-posix::pathname sb-posix::filename
                  (function sb-posix::int (sb-posix::c-string :external-format :ucs-2)
                            (* sb-posix::alien-stat)))

(defun ewstat (name)
  ;;(format t "Processing ~a~%" name) (force-output)
  (handler-case (sb-posix::wstat name)
    (error ()
      (error "Error while accessing file: ~a" name))))

;; because mtime returns unix time we need to convert it to universal time
;; via http://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

;; fixed pathname-as-file from cl-fad
(defun pathname-as-file* (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      ;;(error "Can't reliably convert wild pathnames.")
      (return-from pathname-as-file* pathname))
    (cond ((directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname)))
             (multiple-value-bind (name type) (uiop:split-name-type (car (last directory)))
               (make-pathname :directory (butlast directory)
                              :name name
                              :type type
                              :defaults pathname))))
          (t pathname))))

(defun pathname-as-directory* (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      ;;(error "Can't reliably convert wild pathnames.")
      (return-from pathname-as-directory* pathname))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (pathname-name pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defclass sync-job ()
  ((source :initarg :source :accessor source)
   (target :initarg :target :accessor target)
   (preference :initarg :preference :accessor preference
               :documentation "A keyword for preference rules"
               )
   (processor :initarg :processor :accessor processor
              :documentation
              "A list of 5 elements corresponding to diff fields,
              containing action keywords for each class of files"
              )
   (exclude :initarg :exclude :accessor exclude
            :documentation
            "A list of files not to be synced, may contain wildcards")
   ))

;;sync-job templates

(defun make-job (source target preference processor &optional excludes)
  (make-instance 'sync-job
                 :source source :target target
                 :preference preference
                 :processor processor
                 :exclude excludes))

(defun make-job-overwrite-with-source (source target &optional excludes)
  (make-instance 'sync-job
                 :source source :target target
                 :preference :source
                 :processor (list :copy-source :delete-target :copy-source :copy-source :copy-source)
                 :exclude excludes))

(defun make-job-prefer-newer (source target &optional excludes)
  (make-instance 'sync-job
                 :source source :target target
                 :preference :newer
                 :processor (list :copy-source :ask :copy-source :copy-target :ask)
                 :exclude excludes))

(defun reverse-job (job)
  (psetf (source job) (target job) (target job) (source job)))

(defun wildcard-parse-tree (s)
  (loop for x in (split "(\\*|\\?)" s :with-registers-p t)
       when (string= x "*") collect (list :non-greedy-repetition 0 nil :everything) into res else
       when (string= x "?") collect (list :non-greedy-repetition 0 1 :everything) into res else
       when (> (length x) 0) collect x into res
       finally (return (nconc (list :sequence :start-anchor) res (list :end-anchor)))))

(defun exclude-scanner (list)
  (when list
    (create-scanner (if (> (length list) 1)
                        (cons :alternation (mapcar #'wildcard-parse-tree list))
                        (wildcard-parse-tree (car list)))
                    :case-insensitive-mode t)))

(defclass diff ()
  ((job :initarg :job :accessor job)
   (file-mode :initform nil :accessor file-mode)
   ;;Each of the following is a list of pairs (file1 file2)
   (new-source :initform nil :initarg :new-source :accessor new-source)
   (new-target  :initform nil :initarg :new-source :accessor new-target)
   (prefer-source :initform nil :initarg :prefer-source :accessor prefer-source)
   (prefer-target :initform nil :initarg :prefer-target :accessor prefer-target)
   (undecided :initform nil :initarg :undecided :accessor undecided)))

(defclass file ()
  ((dir :initarg :dir :accessor dir)
   (pname :initarg :pname :accessor pname)
   (name :initarg :name :accessor name)
   (size :initarg :size :accessor size)
   (mtime :initarg :mtime :accessor mtime)
   (ctime :initarg :ctime :accessor ctime)
   (tag :initarg :tag :accessor tag)))

(defgeneric is-file-object (file)
  (:method ((file file)) t)
  (:method ((not-file t)) nil))

(defun make-file-object (pathname &optional tag)
  (let* ((dir (when (directory-pathname-p pathname) t))
         (pn (pathname-as-file* pathname))
         (ns (native-ns pathname))
         (fi (if dir (ewstat (subseq ns 0 (1- (length ns))))
                 (ewstat ns))))
    (make-instance 'file
                   :dir dir
                   :pname pathname
                   :name (file-namestring pn)
                   :size (sb-posix:stat-size fi)
                   :mtime (unix-to-universal-time (sb-posix:stat-mtime fi))
                   :ctime (unix-to-universal-time (sb-posix:stat-ctime fi))
                   :tag tag)))

(defun compare-filenames (file1 file2)
  (string-equal (name file1) (name file2)))

(defun compare-files (file1 file2)
  "Returns nil if files are different"
  (and (compare-filenames file1 file2)
       (eql (dir file1) (dir file2))
       (= (size file1) (size file2))
       (<= (abs (- (mtime file1) (mtime file2))) 7200)
       (<= (mod (abs (- (mtime file1) (mtime file2))) 3600) 5)))

(defgeneric prefer (keyword)
  (:documentation "Returns a function of 2 variables corresponding to
  source and target file object that returns 1 if source file is
  preferred, -1 if target and 0 if undecided"))

(defmethod prefer ((keyword (eql :source)))
  (lambda (source target) (declare (ignore source target)) 1))

(defmethod prefer ((keyword (eql :target)))
  (lambda (source target) (declare (ignore source target)) -1))

(defmethod prefer ((keyword (eql :undecided)))
  (lambda (source target) (declare (ignore source target)) 0))

(defmethod prefer ((keyword (eql :bigger)))
  (lambda (source target)
    (if (or (dir source) (dir target) 0)
        (signum (- (size source) (size target))))))

(defmethod prefer ((keyword (eql :newer)))
  (lambda (source target)
    (if (or (dir source) (dir target) 0)
        (signum (- (mtime source) (mtime target))))))

(defun make-dir (path)
  (pathname-as-directory* (ensure-directories-exist (pathname-as-directory* path))))

(defun build-diff (job)
  (let ((source (source job)) (target (target job))
        (pref (prefer (preference job)))
        (e-scan (exclude-scanner (exclude job)))
        (d (make-instance 'diff :job job))
        (file-mode nil))
    (let ((fs (file-exists-p source))
          (ft (file-exists-p target)))
      (unless (or fs ft)
        (format t "Neither source, nor target exists. Job interrupted.")
        (return-from build-diff))
      (unless (or (and fs (directory-pathname-p fs))
                  (and ft (directory-pathname-p ft)))
        (setf file-mode t (file-mode d) t))
      (unless file-mode
        (unless fs
          (format t "Source directory does not exist. Creating empty.~%")
          (setf fs (make-dir source)))
        (unless ft
          (format t "Target directory does not exist. Creating empty.~%")
          (setf ft (make-dir target)))
        (unless (directory-pathname-p fs)
          (format t "FATAL ERROR: Source directory is not a directory.~%")
          (return-from build-diff))
        (unless (directory-pathname-p ft)
          (format t "FATAL ERROR: Target directory is not a directory.~%")
          (return-from build-diff)))
      (labels ((rec-compare (fs ft)
                 ;;(format t "~a vs ~a~%" fs ft) (force-output)
                 (when file-mode
                   (cond
                     ((not ft) (push (list (make-file-object fs) nil) (new-source d)))
                     ((not fs) (push (list nil (make-file-object ft)) (new-target d)))
                     (t
                      (let ((f1 (make-file-object fs))
                            (f2 (make-file-object ft)))
                        (unless (compare-files f1 f2)
                          (case (funcall pref f1 f2)
                            (1 (push (list f1 f2) (prefer-source d)))
                            (-1 (push (list f1 f2) (prefer-target d)))
                            (0 (push (list f1 f2) (undecided d)))))
                        )))
                   (return-from rec-compare))
                 (let ((h (make-hash-table :test #'equalp))
                       (fsl (list-directory fs))
                       (ftl (list-directory ft)))
                   (loop for f in fsl
                      for fo = (make-file-object f 0)
                      unless (and e-scan (scan e-scan (name fo)))
                      do (push fo (gethash (name fo) h)))
                   (loop for f in ftl
                      for fo = (make-file-object f 1)
                      unless (and e-scan (scan e-scan (name fo)))
                      do (push fo (gethash (name fo) h)))
                   (maphash
                    (lambda (name files)
                      (declare (ignore name))
                      (case (length files)
                        (1 (let ((f (car files)))
                             (case (tag f)
                               (0 (push (list f nil) (new-source d)))
                               (1 (push (list nil f) (new-target d))))))
                        (2 (let ((f2 (first files))
                                 (f1 (second files)))
                             (if (and (dir f1) (dir f2))
                                 (rec-compare (pname f1) (pname f2))
                                 (if (or (dir f1) (dir f2))
                                     (push (list f1 f2) (undecided d))
                                     (unless (compare-files f1 f2)
                                       (case (funcall pref f1 f2)
                                         (1 (push (list f1 f2) (prefer-source d)))
                                         (-1 (push (list f1 f2) (prefer-target d)))
                                         (0 (push (list f1 f2) (undecided d)))))))))))
                    h))))
        (rec-compare fs ft))
      d)))

(defun fv (file path)
  (when file
    (if (directory-exists-p path)
        (subseq (native-ns (pname file))
                (length (native-ns (pathname-as-directory* path))))
        (native-ns (pname file)))))

(defun display-time (time)
  (multiple-value-bind (second minute hour date month year) (decode-universal-time time)
    (format nil "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun preview-diff (diff &aux (source (source (job diff)))
                     (target (target (job diff)))
                     (count 0))
  (format t "~%----------------------------------------------
Synchronisation preview
Source:~a
Target:~a~%"
          source target)
  (flet ((display-pair (src tar act)
           (incf count)
           (format t "~@[Source file:~a~]~@[ ~a~]~@[ ~a~%~]~@[Target file:~a~]~@[ ~a~]~@[ ~a~%~]Default action: ~a~%~%"
                   (fv src source) (when src (size src)) (when src (display-time (mtime src)))
                   (fv tar target) (when tar (size tar)) (when tar (display-time (mtime tar)))
                   act)))
  (when (new-source diff)
    (format t "----------------------------------------------
New source files:
----------------------------------------------~%")
    (loop with act = (first (processor (job diff)))
       for (src tar) in (new-source diff)
       do (display-pair src tar act)))

  (when (new-target diff)
    (format t "----------------------------------------------
New target files:
----------------------------------------------~%")
    (loop with act = (second (processor (job diff)))
       for (src tar) in (new-target diff)
       do (display-pair src tar act)))

  (when (prefer-source diff)
    (format t "----------------------------------------------
Prefer source files:
----------------------------------------------~%")
    (loop with act = (third (processor (job diff)))
       for (src tar) in (prefer-source diff)
       do (display-pair src tar act)))

  (when (prefer-target diff)
    (format t "----------------------------------------------
Prefer target files:
----------------------------------------------~%")
    (loop with act = (fourth (processor (job diff)))
       for (src tar) in (prefer-target diff)
       do (display-pair src tar act)))

  (when (undecided diff)
    (format t "----------------------------------------------
Undecided files:
----------------------------------------------~%")
    (loop with act = (fifth (processor (job diff)))
       for (src tar) in (undecided diff)
       do (display-pair src tar act)))
  count))


(defun native-ns (pathname)
  (sb-ext:native-namestring pathname))

(defun native-as-file (pathname)
  (let ((ns (native-ns pathname)))
    (if (directory-pathname-p pathname)
        (subseq ns 0 (1- (length ns)))
        ns)))

(defun run-command (&rest args)
  (let ((err (uiop:run-program (append '("cmd" "/c") args) :ignore-error-status t)))
    (when (and err (/= err 0)) (format t "Error code:~a~%" err))
    err))

(defun copy-dir (source path)
  (when (and source path)
    (format t "Copying directory ~a to ~a~%" source path)
    (ensure-directories-exist (pathname-as-directory* path))
    (run-command
     "xcopy"
     "/e/i/h/r/y/c/k"
     (native-as-file source)
     (native-as-file path))))

(defun copy-file (source path)
  (when (and source path)
    (format t "Copying file ~a to ~a~%" source path)
    (run-command
     "xcopy"
     "/h/r/k/y"
     (native-ns source)
     (native-as-file (uiop:pathname-directory-pathname path)))))

(defun del-dir (path)
  (when path
    (format t "Deleting directory ~a~%" path)
    (run-command "rd" "/s/q" (native-ns path)) :force-shell t))

(defun del-file (path)
  (when path
    (format t "Deleting file ~a~%" path)
    (run-command "del" "/f" (native-ns path)) :force-shell t))

(defgeneric action (keyword)
  (:documentation "Returns a function of 2 variables corresponding to
  source and target file objects that performs the operation required.
  Source or target may be directory pathnames when applicable"))

(defmethod action ((keyword (eql :nothing)))
  (lambda (source target) (declare (ignore source target)) nil))

(defmethod action ((keyword t))
  (lambda (source target) (declare (ignore source target)) nil))

(defmethod action ((keyword (eql :copy-source)))
  (lambda (source target)
    (block nil
      (unless (is-file-object source) (return))
      (if (dir source)
          (if (is-file-object target)
              ;;target already exists, overwrite. it can only be a file.
              (progn (del-file (pname target))
                     (copy-dir (pname source) (pname target)))
              ;;target is a directory where we should copy the source directory
              (copy-dir (pname source) target))
          ;;source is a file
          (if (is-file-object target)
              ;;target already exists, overwrite. it can be a file or directory.
              (if (dir target)
                  (progn (del-dir (pname target))
                         (copy-file (pname source) (pname target)))
                  (progn (del-file (pname target))
                         (copy-file (pname source) (pname target))))
              ;;target is a path where we should copy the source file
              (copy-file (pname source) target))))))

(defmethod action ((keyword (eql :copy-target)))
  (lambda (target source)
    (block nil
      (unless (is-file-object source) (return))
      (if (dir source)
          (if (is-file-object target)
              ;;target already exists, overwrite. it can only be a file.
              (progn (del-file (pname target))
                     (copy-dir (pname source) (pname target)))
              ;;target is a directory where we should copy the source directory
              (copy-dir (pname source) target))
          ;;source is a file
          (if (is-file-object target)
              ;;target already exists, overwrite. it can be a file or directory.
              (if (dir target)
                  (progn (del-dir (pname target))
                         (copy-file (pname source) (pname target)))
                  (progn (del-file (pname target))
                         (copy-file (pname source) (pname target))))
              ;;target is a path where we should copy the source file
              (copy-file (pname source) target))))))

(defmethod action ((keyword (eql :delete-source)))
  (lambda (source target)
    (declare (ignore target))
    (block nil
      (unless (is-file-object source) (return))
      (if (dir source)
          (del-dir (pname source))
          (del-file (pname source))))))

(defmethod action ((keyword (eql :delete-target)))
  (lambda (target source)
    (declare (ignore target))
    (block nil
      (unless (is-file-object source) (return))
      (if (dir source)
          (del-dir (pname source))
          (del-file (pname source))))))

(defmethod action ((keyword (eql :delete-both)))
  (lambda (source target)
    (when (is-file-object source)
      (if (dir source)
          (del-dir (pname source))
          (del-file (pname source))))
    (when (is-file-object target)
      (if (dir target)
          (del-dir (pname target))
          (del-file (pname target))))))

(defmethod pname ((obj t))
  obj)

(defvar *current-answer* nil)

(defmethod action ((keyword (eql :ask)))
  (lambda (src tar)
    (if *current-answer*
        (let ((act (action *current-answer*)))
          (funcall act src tar))
        (progn
          (format t "What to do?~%")
          (format t "~@[Source file:~a~]~@[ ~a~]~@[ ~a~%~]~@[Target file:~a~]~@[ ~a~]~@[ ~a~]~%"
                  (pname src)
                  (when (is-file-object src) (size src))
                  (when (is-file-object src) (display-time (mtime src)))
                  (pname tar)
                  (when (is-file-object tar) (size tar))
                  (when (is-file-object tar) (display-time (mtime tar))))
          (let ((act (action (read))))
            (funcall act src tar))))))

(defmethod action ((keyword cons))
  "If action is a cons then car of it would serve as the answer to all asks for the current group of files"
  (setf *current-answer* (car keyword))
  (action (car keyword)))

(defmethod action :after (keyword)
  (declare (ignore keyword))
  (force-output))

:copy-source :copy-target :delete-source :delete-target :delete-both

(defun merge-pathnames-relative (target source base)
  (let* ((base-list (cdr (pathname-directory (pathname-as-directory* base))))
         (src-list (cdr (pathname-directory source)))
         (pathname (make-pathname
                    :directory `(:relative ,@(subseq src-list (length base-list)))
                    :defaults source)))
    (uiop:merge-pathnames* pathname (pathname-as-directory* target))))


(defun process-diff (diff &optional processor
                     &aux (source (source (job diff))) (target (target (job diff))))
  (unless processor (setf processor (processor (job diff))))
  (setf *current-answer* nil)
  (loop with act = (action (first processor))
       for (src tar) in (new-source diff)
       do (setf tar (if (file-mode diff) target
                        (merge-pathnames-relative target (pname src) source)))
       (funcall act src tar))
  (setf *current-answer* nil)
  (loop with act = (action (second processor))
       for (src tar) in (new-target diff)
       do (setf src (if (file-mode diff) source
                        (merge-pathnames-relative source (pname tar) target)))
       (funcall act src tar))
  (setf *current-answer* nil)
  (loop with act = (action (third processor))
       for (src tar) in (prefer-source diff)
       do (funcall act src tar))
  (setf *current-answer* nil)
  (loop with act = (action (fourth processor))
       for (src tar) in (prefer-target diff)
       do (funcall act src tar))
  (setf *current-answer* nil)
  (loop with act = (action (fifth processor))
       for (src tar) in (undecided diff)
       do (funcall act src tar))
  (setf *current-answer* nil)
  (format t "Processing complete.~%")
  (force-output))

(defun process-jobs (joblist &optional do-not-ask)
  "When interrupted return current diff and the list of remaining jobs"
  (unless (listp joblist) (setf joblist (list joblist)))
  (loop with flag = t and count
     for job = (pop joblist)
     for diff = (when job (build-diff job))
     while job
     do (format t "~%Processing: ~a ~a ~%" (source job) (target job))
     when diff
     do (setf count (preview-diff diff))
     and unless (or do-not-ask (zerop count))
     do (format t "Process this diff? [(y)es/(s)kip/(q)uit]~%")
       (let ((c (read-char)))
         ;;(print c)
         (cond
           ((char-equal c #\y) (setf flag t))
           ((char-equal c #\s) (setf flag nil))
           (t (return (list diff joblist)))))
       (clear-input)
     when (and diff (or flag do-not-ask)) do (process-diff diff)))

(defun process-jobs-reverse (joblist &optional do-not-ask)
  (unless (listp joblist) (setf joblist (list joblist)))
  (let ((jl (copy-list joblist)))
    (mapcar #'reverse-job joblist)
    (unwind-protect (process-jobs jl do-not-ask)
      (mapcar #'reverse-job joblist))))

(defun quick-sync (source target &key excludes (job-fn #'make-job-overwrite-with-source))
  (let ((jobs (list (funcall job-fn source target excludes))))
    (process-jobs jobs)))
