#!/usr/bin/chicken-csi -ss
;; fisl -- fisl is scheme lox
(load "scanner.scm")
(load "util.scm")

(import (chicken io)
        (chicken base)
        (chicken format)
        scanner
        util)

(define (run code fname)
  (let ((exit-code 0))
    (map print (scan code fname))
    exit-code))

(define (run-prompt)
  (display "> ")
  (let ((l (read-line)))
    (if (not (eof-object? l))
      (begin
        (run l "repl")
        (run-prompt))
      (exit 0))))

(define (run-file fname)
  (call-with-input-file fname (lambda (p)
                                (exit (run (read-string #f p) fname)))))

(define (main args)
  (let ((argc (length args)))
    (cond
      ((eq? argc 0) (run-prompt))
      ((eq? argc 1) (run-file (car args)))
      (else (die "Too many arguments. Usage: fisl [FILE]")))))
