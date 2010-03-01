Exampel
=======
galaxy-parser includes a small sampel program that reads
file names from command line (use -s for standard input),
parses their content and output the haskell datastructures
or a list of parse errors.

To build the program just do
    ghc --make main.hs
in the src/ directory.

Dependencies
============
galaxy-parser should work on the haskell platform

Disclaimer
==========
The actual syntax for galaxy is still rather unknown. galaxy-parser
currently parses all galaxy files that I got my hands on, but the
parse tree has not undergone any closer inspection.
Precedence rules are just guessed.

Author
======
phyrex1an (Mikael Bung)

License
=======
GPL 3