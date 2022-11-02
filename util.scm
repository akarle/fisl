;; util.scm -- shared utils (no deps!)
(import (chicken format)
	(chicken io))

(define had-err #f)

(define (err! str)
  (set! had-err #t)
  (fprintf (current-error-port) "~A\n" str))

(define (clear-err!)
  (set! had-err #f))

(define (die str)
  (err! str)
  (exit 1))
