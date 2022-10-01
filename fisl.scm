#!/usr/bin/chicken-csi -ss
;; fisl -- fisl is scheme lox
(import (chicken io)
	(chicken base)
	(chicken format))

(define (run code)
  (print code))

(define (run-prompt)
  (display "> ")
  (let ((l (read-line)))
    (if (not (eof-object? l))
      (begin
	(run l)
	(run-prompt))
      (exit 0))))

(define (run-file fname)
  (call-with-input-file fname (lambda (p)
     (run (read-string #f p)))))

(define (die str)
  (fprintf (current-error-port) "~A\n" str)
  (exit 1))

(define (main args)
  (let ((argc (length args)))
    (cond
      ((eq? argc 0) (run-prompt))
      ((eq? argc 1) (run-file (car args)))
      (else (die "Too many arguments. Usage: fisl [FILE]")))))
