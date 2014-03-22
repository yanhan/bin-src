#!/bin/bash

# ClojureScript REPL with Readline
# Based on https://github.com/emezeske/lein-cljsbuild/wiki/Using-Readline-with-REPLs-for-Better-Editing

REPL_TO_USE='rhino'

if [[ -n "$1" ]]
then
  REPL_TO_USE=$1
fi

rlwrap -r -m -q '\\"' -b "(){}[],^%3@\\\";:'" lein trampoline cljsbuild repl-$REPL_TO_USE
