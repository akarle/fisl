#!/usr/local/bin/chicken-csi -ss
;; fisl -- fisl is scheme lox
(load "scanner.scm")
(load "util.scm")
(load "parser.scm")
(load "interpreter.scm")

(import (chicken io)
        (chicken base)
        (chicken format)
        scanner
        parser
        interpreter
        util)

(define (run code fname)
  (let ((tokens (scan code fname)))
    (if tokens
	(let ((stmts (parse tokens fname)))
	  (if stmts
	      (print (interpret stmts)))))))

(define (run-prompt)
  (display "> ")
  (let ((l (read-line)))
    (if (not (or (eof-object? l) (equal? l ",q")))
      (begin
        (run l "repl")
        (clear-err!)
        (run-prompt)))))

(define (run-file fname)
  (call-with-input-file fname (lambda (p)
    (run (read-string #f p) fname)
    (exit (if had-err 1 0)))))

(define (main args)
  (let ((argc (length args)))
    (cond
      ((eq? argc 0) (run-prompt) (exit 0))
      ((eq? argc 1) (run-file (car args)))
      (else (die "Too many arguments. Usage: fisl [FILE]")))))
