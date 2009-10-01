;;; load.lisp

; File for fast loading of LispMud project to repl.

(asdf:oos 'asdf:load-op :lispmud :force t)

(in-package :lispmud)