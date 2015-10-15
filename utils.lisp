(defpackage #:utils
  (:use #:common-lisp #:better-handler-case)
  (:export #:suck-stream #:handler-case*))

(in-package #:utils)

(defun suck-stream (stream &key (reader 'read-char))
  (let ((result nil))
    (handler-case*
     (loop do (push (restart-case
			(ecase reader
			  ((read-char) (read-char stream))
			  ((read-byte) (read-byte stream)))
		      (switch-reader ()
			:report "Switch between binary and text readers."
			(setf reader (ecase reader
				       ((read-char) 'read-byte)
				       ((read-byte) 'read-char)))
			(ecase reader
			  ((read-char) (read-char stream))
			  ((read-byte) (read-byte stream)))))
		      result))
     (end-of-file ()
		  :after-unwind
		  (coerce (nreverse result)
			  `(vector ,(ecase reader
					   ((read-char) 'character)
					   ((read-byte) '(unsigned-byte 8)))))))))