;; util.scm -- shared utils (no deps!)
(import (chicken format)
	(chicken io))

(define had-err #f)
(define fname "repl")

(define (set-fname! fn)
  (set! fname fn))

(define (err! str)
  (set! had-err #t)
  (fprintf (current-error-port) "~A\n" str))

(define (fname-err! str)
  (err! (format "~A:~A" fname str)))

(define (clear-err!)
  (set! had-err #f))

(define (die str)
  (err! str)
  (exit 1))
