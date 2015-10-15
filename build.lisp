(load "loader.lisp")
(save-lisp-and-die "icpp" :toplevel #'icpp::cpp-repl :executable t)