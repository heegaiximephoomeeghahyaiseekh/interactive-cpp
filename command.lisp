(defpackage #:command
  (:use #:common-lisp #:utils #:split-sequence #:cffi)
  (:documentation "Functions for running external commands.")
  (:export #:do-command #:do-interactive-command))

(in-package #:command)

(defvar *cmd-background* t)

#+sbcl #-win32
(defun executablep (path)
  "Returns T if the PATH is executable by either the owner, the group,
or others. Does not attempt to figure out if we're the owner or in the group."
  (handler-case
      (let ((mode (sb-posix:stat-mode (sb-posix:stat path)))
	    (isex '(#o100 #o10 #o1)))
	(loop for ex in isex thereis
	     (logand mode ex)))
    (sb-posix:syscall-error () nil)))

#+sbcl #-win32
(defun path-append (dir file)
  (when (stringp dir)
    (setf dir (concatenate 'string dir "/")))
  (merge-pathnames file dir))


#+sbcl #-win32
(defun search-path (cmd)
  (loop for dir in (split-sequence #\: (sb-posix:getenv "PATH"))
       for file = (path-append dir cmd)
       when (executablep file) return file))

(defun cmd (cmd &rest args)
  "Runs an external command."
  #+sbcl (sb-ext:run-program #+win32 (sb-win32::search-path cmd) #-win32 #-sbcl cmd #+sbcl (search-path cmd)
			     args :wait (not *cmd-background*) :output :stream :input nil))

(defun do-command (cmd &rest args)
  "Run a non-interactive command and capture STDOUT to a string."
  (let* ((*cmd-background* t)
	 (process (apply #'cmd (cons cmd args))))
    #+sbcl (values (suck-stream (sb-ext:process-output process))
		   (suck-stream (sb-ext:process-error process))
		   (sb-ext:process-exit-code process))))
					     

#-win32
(defun do-interactive-command (cmd &rest args)
  "Run an interactive command"
  (foreign-funcall "system" :string (format nil "~{ ~a ~}" (cons cmd args))))