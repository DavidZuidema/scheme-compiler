#!/bin/bash
ghc -package parsec -o simple_parser $1
./cleanup
