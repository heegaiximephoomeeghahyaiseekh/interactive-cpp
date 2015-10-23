all: icpp

icpp: *.lisp
	sbcl --load build.lisp
