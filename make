#!/bin/bash
ghc -package parsec -o eval parser-evaluator.hs
./cleanup
