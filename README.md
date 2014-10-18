scheme-compiler
===============

A Scheme compiler written in Haskell. Exercises from [Write Yourself a Scheme in 48 Hours](http://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf)

Usage
-----
 $ ghc -package parsec -o simple_parser simpleparser1.hs 
 $ ./simple_parser "$"

 $ ghc -package parsec -o simple_parser simpleparser2.hs 
 $ ./simple_parser " %"