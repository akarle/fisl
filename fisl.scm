#!/usr/local/bin/chicken-csi -ss
;; fisl -- fisl is scheme lox
(load "scanner.scm")
(load "util.scm")
(load "parser.scm")

(import (chicken io)
        (chicken base)
        (chicken format)
        scanner
        parser
        util)

(define (run code fname)
  (parse)
    (map print (scan code fname)))

(define (run-prompt)
  (display "> ")
  (let ((l (read-line)))
    (if (not (eof-object? l))
      (begin
        (run l "repl")
        (clear-err!)
        (run-prompt))
      (exit 0))))

(define (run-file fname)
  (call-with-input-file fname (lambda (p)
    (run (read-string #f p) fname)
    (exit (if had-err 1 0)))))

(define (main args)
  (let ((argc (length args)))
    (cond
      ((eq? argc 0) (run-prompt))
      ((eq? argc 1) (run-file (car args)))
      (else (die "Too many arguments. Usage: fisl [FILE]")))))
