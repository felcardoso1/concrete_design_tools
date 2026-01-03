

[prev] [prev-tail] [tail] [up]
7.3 Using smart linking

You can compile your units using smart linking. When you use smartlinking, the compiler creates a series of code blocks that are as small as possible, i.e. a code block will contain only the code for one procedure or function.

When you compile a program that uses a smart-linked unit, the compiler will only link in the code that you actually need, and will leave out all other code. This will result in a smaller binary, which is loaded in memory faster, thus speeding up execution.

To enable smartlinking, one can give the smartlink option on the command line: -CX, or one can put the {$SMARTLINK ON} directive in the unit file:
Unit Testunit  
 
{$SMARTLINK ON}  
Interface  
...

Smartlinking will slow down the compilation process, especially for large units.

When a unit foo.pp is smartlinked, the name of the code file is changed to libfoo.a.

Technically speaking, the compiler makes small assembler files for each procedure and function in the unit, as well as for all global defined variables (whether they’re in the interface section or not). It then assembles all these small files, and uses ar to collect the resulting object files in one archive.

Smartlinking and the creation of shared (or dynamic) libraries are mutually exclusive, that is, if you turn on smartlinking, then the creation of shared libraries is turned off. The creation of static libraries is still possible. The reason for this is that it has little sense in making a smartlinked dynamical library. The whole shared library is loaded into memory anyway by the dynamic linker (or the operating system), so there would be no gain in size by making it smartlinked.

[prev] [prev-tail] [front] [up]

Page generated on 2025-07-18.  Report a problem on this page

Source:
https://www.freepascal.org/docs-html/current/prog/progse30.html
