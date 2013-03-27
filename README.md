compilers_hir
=============

An interpreter for Project 3 HIR

It will diff the hir s-expression (output from project 3) with the expected output. If There is a difference, it will optionally attempt to eval each s-expression and diff that.

The results are collected from the python print statements in the program. If the program runs and does not print, the pass/fail result is unknown.

## How to use

Remove `(match (current-command-line-arguments) ...)` from bottom of `pytrans-stub.rkt`
and add `(provide transform-program)` to top of `pytrans-stub.rkt`

To run a file named `test.py` there must be files named `test.py.parsed` and `test.py.trans` in the same directory. 

To diff without interpretation, run

    racket runner.rkt test.py

To diff with interpretation, run

    racket runner.rkt test.py -i

