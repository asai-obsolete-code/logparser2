#!/bin/bash

./plot-allmacro-eval.lisp &
./plot-allmacro-gen.lisp &
./plot-allmacro-exp.lisp &
./plot-mangle-eval.lisp &
./plot-mangle-gen.lisp &
./plot-mangle-exp.lisp &
wait
