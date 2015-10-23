# interactive-cpp (ICPP)
A primitive C++ REPL. I wrote this after failing to build [Cling](http://root.cern.ch/drupal/content/cling), due to not having enough memory to build it.

# What it actually does

ICPP takes snippets of C++ and compiles them into shared-object libraries, which it then loads. If you're defining a variable or function, this library stays loaded and provides that definition to code you type in later.

If you're entering a statement for immediate execution, the statement is wrapped in a function, which is then immediately unloaded and deleted.

If ICPP detects that you're redefining something, the original shared library is deleted, the new one is loaded, and all the libraries that depend on the old one are reloaded.

Every generated C++ source file contains all the C preprocessor definitions you have defined with the `preproc` command, along with declarations for all types defined with `defclass`, `extern` declarations for all variables defined with `declare`, and prototypes for all functions and methods defined via `defun` and `defmethod`.

# Building it.

To build ICPP, you must be on Linux and have Steel Bank Common Lisp ([sbcl.org](http://sbcl.org), `apt-get install sbcl`). The program
uses SBCL-specific features, so it won't work on other Common Lisp implementations without being ported first.

Quicklisp is also required. Find it at [quicklisp.org](http://quicklisp.org/).

The "nm" command must be installed. On Debian, it's in the "binutils" package.

GCC is also required, though you may be able to get it to work with Clang by changing the `*compiler*` and
`*linker*` variables in icpp.lisp. You can also change the `*compiler*`, `*linker*`, and `*source-extension*` variables
to turn it into a C REPL instead of C++. (You would also have to change the source file extension from ".cpp" to ".c" so that GCC treats it as C, and remove the `extern "C" {` block that gets added to throwaway programs).

Once you have everything you need, enter the following at SBCL's `*` prompt:

     (load "build.lisp")

If everything works, you'll end up back in the shell, and there will be an executable called "icpp".

You can also run ICPP from within SBCL. Instead of loading `build.lisp`, load `loader.lisp`, and then
call `(icpp::cpp-repl)`. If you do this from within SLIME, you will not be able to see anything that your C++ code sends to `stdout` or `cout`, since these have nothing to do with Lisp's `*standard-output*`, which is connected to the SWANK socket.

# Using it

ICPP does not contain its own C++ parser. As a result, you must supply additional information for it to know
what to do with your code. For example, the first thing you may want to do is `#include` some header files:

    > preproc #include <stdio.h>

The `preproc` command tells ICPP to store the following preprocessor command and inject it into declarations and
immediately-executed statements that you may specify later.

You may also want to load libraries:

    > load-library /usr/lib/whatever.so

To define a class or struct:

    > defclass class foobar : public other_class {
       public:
          int a;
          int b;
          int c;
          foobar() {
              a = b = c = 0;
          }
    };

`defclass` is also the right command to use to make `using namespace` declarations:

    defclass using namespace std;

To define a variable:

    > declare int x;

This creates a shared-library with the definition for `x`, and also adds the implicit declaration
`extern foobar x;` to any other code you may enter. 

Functions are defined with `defun`:

    defun int my_func(int parameter) {
         return parameter*25;
    }
  
Pre-written C++ source files can be loaded with the `load-source` command:

    > load-source src/my-file.cpp

That links your file as a shared library. You must `#include` its corresponding header file.

As a side effect of the way GCC works, you can also use `load-source` to load `.o` files, and even `.so` files.

To execute a statement immediately, use the `do` command:

    > preproc #include <iostream>
    > defclass using namespace std;
    > do cout << my_func(12) << endl;

Due to buffering, you won't see any output unless you send `endl`.

# Redefining Stuff.

Because ICPP doesn't have a C++ parser, it doesn't know which variables, classes, and functions you have declared,
nor does it know their types. As a result, it is often necessary to manually delete declarations before replacing
them.  There are actually two replacements happening: The declaration/prototype, and the definition. ICPP can
automatically find and replace the definition by using the `nm` command on the shared libraries it creates. It's
the declarations that must be manually deleted.

To delete a declaration, use the `delete` command. It takes no arguments. You will be presented with a numbered menu of
declarations that you can delete. Enter `0` as the number if you end up at this menu by mistake.

## Type changes cannot be detected automatically.

You can, for example, define `int x;` and a function that references it:

    > declare int x;
    > defun int foobar(int y) { return x*y; }

Then you can redefine `x` as a `double`. If you call the function after that without also redefining it (causing the obsolete `int` version of `x` to be referenced), I've found that the program freezes. Sending an interrupt signal brings up the Lisp debugger, from which you can go back to the C++ REPL. 

## Threads cannot be detected.

If a thread is using the code or data in a library, unloading or reloading that library will crash ICPP. ICPP does not know what threads are running or which libraries they depend on.

# Compiler errors.

The compiler's output is captured and put in a text file. If the compiler fails, you will be offered a chance
to view the compiler's stdout or stderr in the editor defined in your $EDITOR environment variable. GCC sends its errors
to stdout.
