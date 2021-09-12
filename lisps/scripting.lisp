(cl:in-package :common-lisp-user)

(require 'asdf)
(require 'uiop)

(defparameter *script-path* (merge-pathnames
                             (or (uiop:getenv "LISP_SCRIPTING_SCRIPT_PATH")
                                 *load-pathname*)
                             (uiop:getcwd)))
(defparameter *script-args* (uiop:command-line-arguments))


(defvar *null-stream* (make-broadcast-stream))


(defvar *supressed-standard-streams* nil)


(defun supress-standard-streams ()
  (let ((null-output (make-broadcast-stream)))
    (unless *supressed-standard-streams*
      (setf *supressed-standard-streams* (list *debug-io*
                                               *error-output*
                                               *query-io*
                                               *standard-input*
                                               *standard-output*
                                               *trace-output*)
            *debug-io* null-output
            *error-output* null-output
            *query-io* null-output
            *standard-input* null-output
            *standard-output* null-output
            *trace-output* null-output))))



(defun restore-standard-streams ()
  (when *supressed-standard-streams*
    (destructuring-bind (standard-output
                         debug-io
                         error-output
                         query-io
                         standard-input
                         trace-output)
        *supressed-standard-streams*
      (setf *supressed-standard-streams* nil
            *debug-io* debug-io
            *error-output* error-output
            *query-io* query-io
            *standard-input* standard-input
            *standard-output* standard-output
            *trace-output* trace-output))))


(defmacro with-suppressed-standard-streams (&body body)
  `(let ((*debug-io* *null-stream*)
         (*error-output* *null-stream*)
         (*query-io* *null-stream*)
         (*standard-input* *null-stream*)
         (*standard-output* *null-stream*)
         (*trace-output* *null-stream*))
     ,@body))


(defun skip-next-shebang ()
  (let ((current-dispatch-handler (get-dispatch-macro-character #\# #\!)))
    (flet ((skip-shebang-line (s c n)
             (declare (ignore c n))
             (read-line s)
             (set-dispatch-macro-character #\# #\! current-dispatch-handler)
             (values)))
      (set-dispatch-macro-character #\# #\! #'skip-shebang-line))))


(defun load-silently (pathspec &key external-format)
  (apply #'load pathspec
         :verbose nil
         :print nil
         (append
          (when external-format
            (list :external-format external-format)))))


(defun load-relative (pathspec &key external-format)
  (load-silently (merge-pathnames pathspec *script-path*) :external-format external-format))


(defun keywordify (name)
  (uiop:intern* (uiop:standard-case-symbol-name name) :keyword))


(defun trim (name)
  (string-trim '(#\Tab #\Space #\Newline) (string name)))


(defun string* (control &rest args)
  (apply #'format nil control args))


(defun string+ (&rest args)
  (format nil "~{~A~}" args))


(defvar *shout-stream* *standard-output*)

(defun shout (control &rest params)
  (handler-case
      (format *shout-stream* "~&~A~&" (apply #'format nil (string control) params))
    (serious-condition (c)
      (warn "Failed to shout `~A` with arguments ~A: ~A" control params c)))
  (finish-output t))


(defun shout-error (control &rest params)
  (let ((*shout-stream* *error-output*))
    (apply #'shout control params)))


(defun dir (base &rest pathnames)
  (flet ((ensure-relative-dir (dir)
           (uiop:ensure-directory-pathname (uiop:enough-pathname dir "/"))))
    (reduce #'merge-pathnames (nreverse (mapcar #'ensure-relative-dir pathnames))
            :initial-value (uiop:ensure-directory-pathname base)
            :from-end t)))


(defun file (&rest pathnames)
  (flet ((ensure-file (pathname)
           (let ((pathname (pathname pathname)))
             (if (uiop:directory-pathname-p pathname)
                 (let* ((dir (pathname-directory pathname))
                        (namepath (pathname (first (last dir)))))
                   (make-pathname :directory (butlast dir)
                                  :name (pathname-name namepath)
                                  :type (pathname-type namepath)
                                  :defaults pathname))
                 pathname))))
    (multiple-value-bind (neck last)
        (loop for (path . rest) on pathnames
              if rest
                collect path into neck
              else
                return (values neck (ensure-file path)))
      (if neck
          (merge-pathnames (uiop:enough-pathname last "/") (apply #'dir neck))
          last))))


(defmacro with-temporary-directory ((&key pathname) &body body)
  (let ((tmp-file (gensym))
        (tmp-dir (gensym)))
    `(uiop:with-temporary-file (:pathname ,tmp-file)
       (let* ((,tmp-dir (merge-pathnames (format nil "~A.dir/" (pathname-name ,tmp-file))
                                         (uiop:pathname-directory-pathname ,tmp-file)))
              ,@(when pathname
                  `((,pathname ,tmp-dir))))
         (unwind-protect
              (progn
                (ensure-directories-exist ,tmp-dir)
                ,@body)
           (uiop:delete-directory-tree ,tmp-dir :validate (constantly t)))))))


(defmacro bind-arguments (&body params)
  (multiple-value-bind (optional keys)
      (loop for (param . rest) on params
            until (and (atom param) (string= param '&key))
            collect param into optional-params
            finally (return (values optional-params rest)))
    (multiple-value-bind (args value-map)
        (loop with value-map = (make-hash-table)
              with values = nil
              for args = *script-args* then (rest args)
              for arg = (first args)
              while args
              do (cond
                   ((uiop:string-prefix-p "--" (trim arg))
                    (setf (gethash (keywordify (subseq (trim arg) 2)) value-map) (second args)
                          args (rest args)))
                   ((find-if (lambda (key)
                               (destructuring-bind (full-name &rest things)
                                   (uiop:ensure-list key)
                                 (declare (ignore things))
                                 (destructuring-bind (key &rest things)
                                     (uiop:ensure-list full-name)
                                   (declare (ignore things))
                                   (eql key arg))))
                             keys)
                    (setf (gethash arg value-map) (second args)
                          args (rest args)))
                   (t (uiop:appendf values (list arg))))
              finally (return (values values value-map)))
      `(progn ,@(loop for opt in optional
                      for rest-args = args then (rest rest-args)
                      collect (destructuring-bind (name &optional default-value)
                                  (uiop:ensure-list opt)
                                `(defparameter ,name
                                   ,(or (first rest-args) default-value))))
              ,@(loop for key in keys
                      append (destructuring-bind (&optional full-name
                                                    default-value
                                                    provided-p)
                                 (uiop:ensure-list key)
                               (destructuring-bind (designator &optional name)
                                   (uiop:ensure-list full-name)
                                 (multiple-value-bind (value found-p)
                                     (gethash (keywordify designator) value-map)
                                   `((defparameter ,(or name designator) ,(if found-p
                                                                              value
                                                                              default-value))
                                     ,@(when provided-p
                                         `((defparameter ,provided-p ,found-p))))))))))))


(defun parse-boolean (arg)
  (cond
    ((uiop:emptyp arg)
     nil)
    ((eq arg t) t)
    ((and (stringp arg)
          (member (string-downcase arg) '("t" "true" "yes" "on") :test #'equal))
     t)
    ((and (stringp arg)
          (member (string-downcase arg) '("nil" "false" "no" "off") :test #'equal))
     nil)
    (t (error "Unrecognized boolean value: ~A" arg))))


(defparameter *supress-errors* nil)
(defparameter *trim-output* t)
(defparameter *shell-output* nil)


(define-condition shell-command-error (serious-condition)
  ((code :initarg :code)))


(defmacro return-code-on-error (&body body)
  `(handler-case
       (progn ,@body)
     (shell-command-error (e) (slot-value e 'code))))


(defun $ (&rest args)
  (flet ((quote-arg (arg)
           (cond
             ((stringp arg) (string+ "\"" arg "\""))
             ((keywordp arg) (string* "~(~A~)" arg))
             ((pathnamep arg) (string+ "'" (uiop:native-namestring arg) "'"))
             (t (string arg)))))
    (let ((command (format nil "~{~A~^ ~}" (mapcar #'quote-arg args))))
      (multiple-value-bind (std err code)
          (uiop:run-program command :output (or *shell-output* :string)
                                    :error-output (unless *supress-errors*
                                                    *error-output*)
                                    :force-shell t
                                    :ignore-error-status t)
        (declare (ignore err))
        (if (= code 0)
            (if *trim-output*
                (trim std)
                std)
            (make-condition 'shell-command-error :code code))))))


(defun read-safely (string)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (with-input-from-string (in string)
        (read in)))))


(defun enable-features (&rest features)
  (setf *features* (nunion *features* features :test #'equal)))


(defun disable-features (&rest features)
  (setf *features* (nset-difference *features* features :test #'equal)))


(defmacro with-features ((&rest features) &body body)
  `(let ((*features* (union *features* (list ,@features) :test #'equal)))
     ,@body))


(defun real-time-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second) 0d0))


(defun wait-for-pathname (pathname &key remove timeout (sleep 1))
  (flet ((pathname-exists-p ()
           (if (uiop:directory-pathname-p pathname)
               (uiop:directory-exists-p pathname)
               (uiop:file-exists-p pathname)))
         (remove-pathname ()
           (if (uiop:directory-pathname-p pathname)
               (uiop:delete-directory-tree pathname :validate (constantly t) :if-does-not-exist :ignore)
               (uiop:delete-file-if-exists pathname))))
    (loop with start-time = (real-time-seconds)
          for exists = (pathname-exists-p)
          until (or exists
                    (and timeout (> (- (real-time-seconds) timeout) start-time)))
          do (sleep sleep)
          finally (when exists
                    (when remove
                      (remove-pathname))
                    (return t)))))
