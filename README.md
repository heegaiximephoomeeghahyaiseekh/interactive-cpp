# interactive-cpp
A primitive C++ REPL.

To build it, you must be on Linux and have Steel Bank Common Lisp ([sbcl.org](http://sbcl.org), `apt-get install sbcl`). The program
uses SBCL-specific features, so it won't work on other Common Lisp implementations without being ported first.

Quicklisp is also required. Find it at [quicklisp.org](http://quicklisp.org/).

The "nm" command must be installed. On Debian, it's in the "binutils" package.

GCC is also required, though you may be able to get it to work with Clang by changing the `*compiler*` and
`*linker*` variables in icpp.lisp. You can also change the `*compiler*`, `*linker*`, and `*source-extention*` variables
to turn it into a C REPL instead of C++.

Once you have everything you need, enter the following at SBCL's `*` prompt:

     (load "build.lisp")

If everything works, you'll end up back in the shell, and there will be an executable called "icpp".

# Using it

ICPP does not contain an actual C++ parser. As a result, you must supply additional information for it to know
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

## Be careful.

You can, for example, define `int x;` and a function that references it. Then you can redefine `x` as a `double`.
If you call the function after that without also redefining it, I've found that the program freezes. Sending an
interrupt signal brings up the Lisp debugger, from which you can go back to the C++ REPL. Lisp also attempts
to capture SIGSEGV errors, but the program will be very unstable after a segfault.

# Compiler errors.

The compiler's output is captured and put in a text file. If the compiler fails, you will be offered a chance
to view the compiler's stdout or stderr in the editor defined in your $EDITOR environment variable. GCC sends its errors
to stdout.
