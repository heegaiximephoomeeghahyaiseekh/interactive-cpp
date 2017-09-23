(defpackage #:icpp-user)
(defpackage #:icpp
  (:use #:common-lisp #:cffi #:temporary-file #:command #:better-handler-case #:alexandria))

(in-package #:icpp)

(defparameter *prototypes* nil)
(defparameter *compiler-directives* nil)
(defparameter *declarations* nil)
(defparameter *classes* nil)
(defparameter *compiler* "g++" "Determines the compiler to run. Default is g++.")
(defparameter *linker* "g++" "Determines the program used to link executables. Default is g++")
(defparameter *c-compiler* "gcc" "The C compiler.")
(defparameter *c-linker* "gcc")
(defparameter *c++-compiler* "g++")
(defparameter *c++-linker* "g++")
(defparameter *c-mode* nil)
(defparameter *source-extension* ".cpp" "Determines the extension for source files.")
(defparameter *binary-extension* #+(or win32 windows win64) ".dll" #-(or win32 windows win64) ".so")
(defparameter *shared-flag* "-shared" "The flag used to tell the compiler/linker to produce a shared library.
Default is -shared, the flag accepted by G++.")
(defparameter *output-flag* "-o")
(defparameter *declaration-libs* nil)
(defparameter *function-libs* nil)
(defparameter *symbol-lookup* (make-hash-table) "Look up the library that defines a given C symbol")
(defparameter *dependents* (make-hash-table) "Look up the libraries that depend on a given library.")
(defparameter *loaded-libraries* nil)
(defparameter *ignored-symbols* '(__bss_start _end _fini _init) "Symbols other than those defined by us that are defined by shared-object libraries and should be ignored for dependency-analysis purposes.")
(defparameter *loaded-source-files* nil)

(defun include-flag ()
  (concatenate 'string "-I" (sb-posix:getcwd)))

(defun tempname (extension)
  (with-output-to-temporary-file (out :template (format nil "TEMPORARY-FILES:%~a" extension)) nil))

(define-condition compiler-error (simple-error) ((stdout :initarg :stdout) (stderr :initarg :stderr) (exit-code :initarg :exit-code)))
(define-condition user-error (simple-error) ())

(defun toggle-c-mode ()
  (setf *c-mode* (not *c-mode*))
  (cond (*c-mode*
	 (setf *compiler* *c-compiler*
	       *linker* *c-linker*
	       *source-extension* ".c"))
	(t
	 (setf *compiler* *c++-compiler*
	       *linker* *c++-linker*
	       *source-extension* ".cpp"))))

(defun write-declarations (stream &key (extern t))
  (loop for decl in (reverse *classes*) do
       (write-string decl stream)
       (terpri stream))
  (loop for decl in (reverse *declarations*) do
       (when extern
	 (write-string "extern " stream))
       (write-string decl stream)
       (terpri stream))
  (loop for decl in (reverse *prototypes*) do
       (write-string decl stream)
       (terpri stream)))

(defun write-directives (stream)
  (loop for directive in (reverse *compiler-directives*) do
       (write-string directive stream)
       (terpri stream)))

(defparameter *timer-function*
  "#include <sys/time.h>
#include <time.h>
#include <errno.h>
#include <stdio.h>
unsigned long long microtime() {
   struct timeval tv;
   unsigned long long retval;
   errno=0;
   if(gettimeofday(&tv, 0) == -1) {
     return 0;
   }
   retval = tv.tv_sec*1000000;
   retval += tv.tv_usec;
   return retval;
}

")

(defun write-timer-program (statement stream &optional eval)
  (declare (ignore eval))
  (write-string *timer-function* stream)
  (terpri stream)
  (write-throwaway-program
   (concatenate 'string
    "unsigned long long _icpp_wall_time_start = microtime();
clock_t _icpp_cpu_time_start = clock();
  unsigned long long _icpp_wall_time_end;
  clock_t _icpp_cpu_time_end;

" statement "

  _icpp_cpu_time_end = clock();
  _icpp_wall_time_end = microtime();
  printf(\"CPU Time: %f sec\\n\", (double)(_icpp_cpu_time_end - _icpp_cpu_time_start)/CLOCKS_PER_SEC);
  printf(\"Elapsed time: %f sec\\n\", (double)(_icpp_wall_time_end - _icpp_wall_time_start)/1000000.0);
") stream))


(defun write-throwaway-program (statement stream &optional eval)
  (if eval
      (if *c-mode*
	  (error "EVAL not available in C mode.")
	  (format stream "#include <iostream>~%")))
  (write-directives stream)
  (write-declarations stream)
  (unless *c-mode*
    (write-string "extern \"C\" {" stream)
    (terpri stream))
  (write-string "void user_statement() {" stream)
  (terpri stream)
  (cond (eval
	 (write-string "std::cout << " stream)
	 (write-string statement stream)
	 (terpri)
	 (write-string "std::cout << std::endl;" stream))
	(t
	 (write-string statement stream)))
  (terpri stream)
  (write-string "}" stream)
  (unless *c-mode*
    (write-string "}" stream))
  (terpri stream))

(defun split (delim seq)
  (and seq (split-sequence:split-sequence delim seq)))

(defun compile-cpp-file (source-file binary-file)
  (multiple-value-bind (stdout stderr exit-code)
	(apply #'do-command `(,*compiler*
			    ,@(remove "" (split #\Space (sb-posix:getenv "CFLAGS")) :test #'equalp)
	  ,*shared-flag* ,(include-flag) ,(namestring source-file) ,*output-flag* ,(namestring binary-file)))
      (unless (eq exit-code 0)
	(error 'compiler-error
	       :stdout stdout
	       :stderr stderr
	       :exit-code exit-code
	       :format-control "Compiler error!"))))

(defun load-source-file (source-file)
  (declare (type (or pathname string) source-file))
  (let ((binary-file (tempname *binary-extension*)))
    (compile-cpp-file source-file binary-file)
    (let ((lib (register-library binary-file)))
      (push (cons (if (stringp source-file)
		      source-file
		      (namestring source-file))
		  lib) *loaded-source-files*)
      lib)))

(defun unload-source-file (name)
  (let* ((entry (assoc name *loaded-source-files* :test #'equalp))
	 (lib (cdr entry)))
    (unregister-library lib)
    (delete-file (foreign-library-pathname lib))
    (setf *loaded-source-files*
	  (remove entry *loaded-source-files*))))
    
  
(defun search-set (candidates sequence &key (test #'eql))
  (loop for c in candidates
       thereis (search c sequence :test test)))

(defun read-symbols (sofile)
  "Use the 'nm' Unix tool to extract symbols defined or needed by a shared object."
  (declare (type (or pathname string) sofile))
  (when (pathnamep sofile)
    (setf sofile (namestring sofile)))
  (with-input-from-string (in (do-command "nm" sofile))
    (loop for line = (read-line in nil nil)
	 while line
	 when (search-set '(" T " " U " " B ") line)
       collect (nreverse
		(handler-case
		    (read-from-string			  
		     (concatenate 'string "(" line ")"))
		  (sb-kernel:reader-impossible-number-error ()
		    (read-from-string
		     (concatenate 'string "(#x" line ")"))))))))

(defun get-defined-symbols (symbol-list)
  "Given a list of symbol data returned by READ-SYMBOLS, return those symbols
that are defined by the shared object that was read."
  (loop for (name type) in symbol-list
       when (member type '(t b)) collect name))

(defun get-needed-symbols (symbol-list)
  (loop for (name type) in symbol-list
       when (eq type 'u) collect name))

(defun dependents (lib)
  (declare (type foreign-library lib))
  (reverse 
   (gethash lib *dependents* nil)))

(defun add-dependent (lib dependent)
  (declare (type foreign-library lib dependent))
  (push dependent (gethash lib *dependents* nil)))

(defun symbol-name= (a b)
  (equalp (symbol-name a) (symbol-name b)))

(defun remove-all (items sequence)
  (remove-if (lambda (el)
	       (member el items :test #'symbol-name=)) sequence))

(defun check-duplicate-definitions (symbol-list)
  (loop for symb in (remove-all *ignored-symbols* symbol-list)
     for lib = (gethash symb *symbol-lookup* nil)
     when lib collect lib))

(defun unregister-library (lib)
  (let ((collateral-unloads
	 (loop for d in (dependents lib)
	    collect (foreign-library-pathname d)
	    append (unregister-library d))))
    (close-foreign-library lib)
    (remhash lib *dependents*)
    (loop for k being the hash-keys in *symbol-lookup*
	 when (eq (gethash k *symbol-lookup*) lib)
	 do (remhash k *symbol-lookup*))
    (setf *loaded-libraries* (remove lib *loaded-libraries* :key #'car))
    collateral-unloads))

(defun swap-library (old-lib new-lib &key no-delete)
  (declare (type foreign-library old-lib)
	   (type (or string pathname) new-lib))
  (let ((pending-reloads (unregister-library old-lib)))
    (ignore-errors (delete-file (foreign-library-pathname old-lib)))
    (register-library new-lib :no-delete no-delete)
    (loop for lib in pending-reloads do
	 ;; FIXME: We can't tell if each LIB was originally loaded
	 ;; with :NO-DELETE or not.
	 (register-library lib))))

(defun register-library (so-file &key no-delete)
  (declare (type (or pathname string) so-file))
  (when (pathnamep so-file)
    (setf so-file (namestring so-file)))
  (let* ((symbols (read-symbols so-file))
	 (defined (get-defined-symbols symbols))
	 (needed (get-needed-symbols symbols))
	 (conflicts (check-duplicate-definitions defined)))
    (when conflicts
      ;; Will trigger REGISTER-LIBRARY to be called recursively
      ;; with the one library removed.
      (swap-library (car conflicts) so-file :no-delete no-delete))
    (unless (find so-file *loaded-libraries* :key (compose #'foreign-library-pathname #'car) :test #'equalp)
      (let ((lib
	     (handler-case*
	      (load-foreign-library so-file)
	      (t (exn) :before-unwind
		 (unless no-delete
		   (delete-file so-file))))))
	(unless no-delete
	  (push (list lib no-delete) *loaded-libraries*))
	(loop for symb in defined do
	     (setf (gethash symb *symbol-lookup*) lib))
	(loop for symb in needed do
	     (setf (gethash
		     (gethash symb *symbol-lookup*) *dependents*) lib))
	lib))))    

(defun preprocess (directive)
  (push directive *compiler-directives*))

(defun read-cpp-directive (stream)
  (with-output-to-string (out)
    (loop for ch = (read-char stream)
       do (write-char ch out)
	 (case ch
	   ((#\\) (write-char (read-char stream) out))
	   ((#\") (write-string (read-cpp-string stream) out))
	   ((#\Newline) (return))))))

(defun cpp-execute (statement &key eval (write-program 'write-throwaway-program))
  "Executes a throwaway C++ statement, which gets wrapped in a function. The statement is assumed to run in void context."
  (let* ((source-file
	  (with-output-to-temporary-file (out :template (format nil "TEMPORARY-FILES:%~a" *source-extension*))
	    (funcall write-program statement out eval)))
	 (binary-file (tempname *binary-extension*)))
    (compile-cpp-file source-file binary-file)
    (let ((lib (load-foreign-library binary-file)))
      (unwind-protect
	   (foreign-funcall "user_statement" :void)
	(progn
	  (close-foreign-library lib)
	  (delete-file source-file)
	  (delete-file binary-file))))))

(defun cpp-declare (declaration &key is-class)
  (let* ((source-file 
	  (with-output-to-temporary-file (out :template (format nil "TEMPORARY-FILES:%~a" *source-extension*))
	    (write-directives out)
	    (write-declarations out)
	    (write-string declaration out)))
	 (binary-file (tempname *binary-extension*)))
    (unwind-protect
	 (compile-cpp-file source-file binary-file)
      (delete-file source-file))
    (if is-class
	(push declaration *classes*)
	(push declaration *declarations*))
    (push (cons declaration (register-library binary-file)) *declaration-libs*)))

(defun cpp-defun (prototype function-def)
  (let ((source-file (with-output-to-temporary-file (out :template (format nil "TEMPORARY-FILES:%~a" *source-extension*))
		       (write-directives out)
		       (write-declarations out)
		       (write-string function-def out)
		       (terpri out)))
	(binary-file (tempname *binary-extension*)))
    (unwind-protect
	 (compile-cpp-file source-file binary-file)
      (delete-file source-file))    
    (push (cons prototype (register-library binary-file)) *function-libs*)
    (when prototype
      (push prototype *prototypes*))))

(defun read-cpp-string (stream)
  (with-output-to-string (out)
    (loop for ch = (read-char stream)
       do (write-char ch out)
	 (case ch
	   ((#\\) (let ((next-char (read-char stream)))
		    (case next-char
		      ((#\x) (write-char next-char)
		             (write-char (read-char stream) out))
		      (otherwise (write-char next-char out)))))
	   ((#\") (return))))))

(defun naive-cpp-read (stream &key function-mode)
  "Reads a C++ statement, attempting to figure out where it ends. Returns the whole thing."
  (declare (optimize (debug 3)))
  (let ((paren-level 0)
	(brace-level 0)
	(seen-brace nil)
	(prototype nil)
	(seen-chars nil))
    (declare (optimize (debug 3)))
    (values
     (with-output-to-string (out)
      (loop for ch = (read-char stream)
	   do (write-char ch out)
	   (when (and function-mode
		      (not seen-brace))
	     (push ch seen-chars))
	   (case ch
	     ((#\") (write-string (read-cpp-string stream) out))
	     ((#\() (incf paren-level))
	     ((#\)) (decf paren-level))
	     ((#\{) (incf brace-level)
	            (when (and function-mode (not seen-brace))
		      (setf prototype (coerce (reverse (cons #\; (rest seen-chars))) 'string)))
	            (setf seen-brace t))
	     ((#\}) (decf brace-level)
	            (when (and function-mode
			       seen-brace
			       (= 0 paren-level brace-level)
			       (return))))
	     ((#\;) (when (or (and (or (not function-mode)
				       (and function-mode seen-brace))
				   (= 0 paren-level brace-level)))
		      (return))))))
     prototype)))

(defparameter *repl-help*
  '((defun "<function-definition>"
	"Define a function. " "defun int my_func(int y) {
                return y*2;
             }")
    (defmethod "<method-definition>"
	"Define a method." "defmethod int my_class::my_method(int y) {
                return m_x*y;
             }")
    (preproc "<preprocessor-directive>"
        "Add a preprocessor directive. Affects all future compilation."
     "preproc #include <stdio.h>")
    (declare "<variable-declaration>;"
        "Declare a variable (ICPP will add `extern' in front of it in some situations).")
    (delete "" "Delete a declaration.")
    (declarations "" "Show all active declarations and preprocessor directives.")
    (load-source "<filename>" "Compile a source file and load it in.")
    (unload-source "<filename>" "Unload a previously loaded source file.")
    (load-library "<filename>" "Link to a shared-object library.")
    (c-mode "" "Toggle between C and C++ (default is C++)")
    (defclass "<class-definition>;"
	"Add a class or struct definition, or for things like `using namespace std'" "defclass class my_class { public: int x; };")
    (do "<statement>;" "Execute a statement.")
    (time "<statement>;" "Measure the execution time of a statement.")
    (eval "<expression>;" "Evaluate an expression (C++ only). Note: The semicolon is required.")
    (quit "" "Quit ICPP.")))

(defun print-declarations (&key numbered)
  (let ((n 0))
    (format t "Preprocessor directives:~%")
    (loop for directive in (reverse *compiler-directives*)
       do (incf n)
	 (format t "~2@a    ~@a" (if numbered n "") directive))
    (format t "~%Declarations:~%")
    (loop for decl in (reverse *declarations*)
       do (incf n)
	 (format t "~2@a    ~a~%" (if numbered n "") decl))
    (format t "~%Prototypes:~%")
    (loop for prot in (reverse *prototypes*)
       do (incf n)
	 (format t "~2@a    ~a~%" (if numbered n "") prot))
    (format t "~%Classes/Structs:~%")
    (loop for class in (reverse *classes*) do
	 (incf n)
	 (format t "~2@a    ~a~%" (if numbered n "") class))))

(defun delete-declaration ()
  (print-declarations :numbered t)
  (format t "Enter the number of the declaration to delete: ")
  (finish-output)
  (let ((target (read))
	(n 0))
    (macrolet ((delete-from (var)
		 `(loop for item in (reverse ,var)
		      do (incf n)
		      when (= n target) do
		      (setf ,var (remove item ,var))
		      (return-from delete-declaration))))
      (delete-from *compiler-directives*)
      (delete-from *declarations*)
      (delete-from *prototypes*)
      (delete-from *classes*)))) 

(defun read-cmd ()
  (let* ((cmd (let ((*package* (find-package '#:icpp-user)))
	       (read)))
	(c++-code (case cmd
		    ((icpp-user::preproc)
		     (read-cpp-directive *standard-input*))
		    ((icpp-user::load-source icpp-user::unload-source icpp-user::load-library)
		     (read-line))
		    ((icpp-user::defun icpp-user::defmethod icpp-user::help icpp-user::quit icpp-user::declarations
		      icpp-user::delete icpp-user::c-mode)
		     nil)
		    (otherwise 
		     (naive-cpp-read *standard-input*)))))
				  
    (case cmd
      ((icpp-user::load-source)
       (load-source-file c++-code))
      ((icpp-user::unload-source)
       (unload-source-file c++-code))
      ((icpp-user::load-library)
       (register-library c++-code :no-delete t))
      ((icpp-user::defun icpp-user::defmethod)
       (multiple-value-bind (function-def prototype) (naive-cpp-read *standard-input* :function-mode t)
	 (cpp-defun (and (eq cmd 'icpp-user::defun) prototype) function-def)))
      ((icpp-user::declarations)
       (print-declarations))
      ((icpp-user::delete)
       (delete-declaration))
      ((icpp-user::preproc)
       (preprocess c++-code))
      ((icpp-user::defclass)
       (cpp-declare c++-code :is-class t))
      ((icpp-user::declare)
       (cpp-declare c++-code))
      ((icpp-user::do)
       (cpp-execute c++-code))
      ((icpp-user::eval)
       (cpp-execute c++-code :eval t))
      ((icpp-user::time)
       (cpp-execute c++-code :write-program 'write-timer-program))
      ((icpp-user::c-mode)
       (toggle-c-mode)
       (format t "C mode is ~a~%" (if *c-mode* "ON" "OFF")))
      ((icpp-user::help)
       (loop for (symbol parameters description example) in *repl-help*
	    do (format t "~12a~25@a~%    ~a~%~%" symbol parameters description)
	       (when example (format t "    Example: ~a~%~%" example))))
      ((icpp-user::quit)
       #+sbcl (throw 'exit nil))
      (otherwise
       (error 'user-error :format-control "Unknown command: ~a~%Type HELP for a list of commands.~%"
	      :format-arguments (list cmd))))))

(defun edit-string (string)
  (let ((filename (with-output-to-temporary-file (out)
		    (write-string string out))))
    (unwind-protect
	 (do-interactive-command (sb-posix:getenv "EDITOR") (namestring filename))
      (delete-file filename))))

(defun handle-compiler-error (exn)
  (format t "Compiler returned exit code ~a~%" (slot-value exn 'exit-code))
  (let ((menu `((1 "View the STDOUT output"
		   ,(lambda ()
			    (edit-string (slot-value exn 'stdout))))
		(2 "View the STDERR output"
		   ,(lambda ()
			    (edit-string (slot-value exn 'stderr))))
		(3 "Return to the REPL"
		   ,(lambda ()
			    (invoke-restart 'c++-repl))))))
    (loop do
	 (loop for (num name lambda) in menu	    
	    do (format t "  ~a. ~a~%" num name))
	 (let* ((n (read))
		(proc (car (last (assoc n menu)))))
	   (if proc (funcall proc))))))

(defun cpp-repl ()
  (format t "ICPP Version 0.5~%Type 'HELP' for a list of commands.~%Most commands must end with a semicolon.~%~%")
  (handler-case*
   (unwind-protect
	(catch 'exit
	  (loop do
	       (restart-case
		   (progn
		     (format t ">")
		     (finish-output)
		     (read-cmd))
		 (c++-repl () :report "Return to the C++ REPL"
			   nil))))
     (progn
       (loop for (lib no-delete) in *loaded-libraries*
	  do (close-foreign-library lib)
	    (unless no-delete
	      (handler-case 
		  (delete-file (foreign-library-pathname lib))
		(t ()
		  nil))))))
   (style-warning (exn)
       :before-unwind (invoke-restart 'muffle-warning))
   (compiler-error (exn)
       :before-unwind (handle-compiler-error exn))))

(defun hash->alist (hash)
  (let (result)
    (maphash (lambda (k v)
	       (push (cons k v) result)) hash)
    result))
