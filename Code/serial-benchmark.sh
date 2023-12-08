#!/bin/sh
sbcl \
    --eval '(ql:quickload :platypus-games-without-frontiers)' \
    --eval '(save-lisp-and-die "/tmp/benchmark" :toplevel (function pgwf::serial-random-benchmark) :executable t)'
echo --- Random ---
perf stat /tmp/benchmark
sbcl \
    --eval '(ql:quickload :platypus-games-without-frontiers)' \
    --eval '(save-lisp-and-die "/tmp/benchmark" :toplevel (function pgwf::serial-sequential-benchmark) :executable t)'
echo --- Sequential ---
perf stat /tmp/benchmark
