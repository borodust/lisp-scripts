(defun default-quicklisp-home ()
  (dir (or (uiop:getenv "QUICKLISP_HOME")
           (uiop:getenv "HOME")
           (uiop:get-pathname-defaults))
       "quicklisp/"))


(defun quicklisp-init (&optional quicklisp-home)
  (let ((setup-file (file (or quicklisp-home
                              (default-quicklisp-home))
                          "setup.lisp")))
    (when (uiop:probe-file* setup-file)
      (load-silently setup-file)
      setup-file)))
