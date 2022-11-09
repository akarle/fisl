#!/usr/local/bin/chicken-csi -ss
;; fisl -- fisl is scheme lox
(import srfi-18
        nrepl
        (chicken io)
        (chicken repl)
        (chicken base)
        (chicken format))

(include "util.scm")
(include "scanner.scm")
(include "parser.scm")
(include "interpreter.scm")

(define (run code fname)
  (let ((tokens (scan code fname)))
    (if tokens
	(let ((stmts (parse tokens fname)))
	  (if stmts
	      (interpret stmts))))))

(define (prompt)
  ;; HACK: srfi-18 blocks for IO, so having run-prompt
  ;; use read-line means that nrepl just doesn't work :(
  ;; the "solution" is to set the i/o to non-blocking
  ;; (adapted from the srfi-18 egg itself)
  (display "> ")
  (flush-output)
  (##sys#thread-block-for-i/o! ##sys#current-thread 0 #:input)
  (thread-yield!))

(define (run-prompt)
  (parameterize ((repl-prompt "> "))
    (prompt)
    (let ((l (read-line)))
      (if (not (or (eof-object? l) (equal? l ",q")))
        (begin
          (run l "repl")
          (clear-err!)
          (run-prompt))))))

(define (run-file fname)
  (call-with-input-file fname (lambda (p)
    (run (read-string #f p) fname)
    (exit (if had-err 1 0)))))

(define (main args)
  (let ((argc (length args)))
    (cond
      ((eq? argc 0)
       (thread-start! (lambda () (run-prompt) (exit 0)))
       (print "starting the repl")
       (nrepl 1234))
      ((eq? argc 1) (run-file (car args)))
      (else (die "Too many arguments. Usage: fisl [FILE]")))))
