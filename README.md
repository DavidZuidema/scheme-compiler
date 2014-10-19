scheme-compiler
===============

A Scheme compiler written in Haskell. Exercises from [Write Yourself a Scheme in 48 Hours](http://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf)

Usage
-----
$ ./make simpleparser1.hs 
$ ./simple_parser "$"

$ ./make simpleparser2.hs 
$ ./simple_parser " %"

$ ./make datatypeparser.hs 
$ ./simple_parser "\"this is a string\""
$ ./simple_parser 25
$ ./simple_parser symbol