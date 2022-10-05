(module util (die err! had-err clear-err!)
  (import scheme
          (chicken base)
          (chicken io)
          (chicken format))

  (define had-err #f)

  (define (err! str)
    (set! had-err #t)
    (fprintf (current-error-port) "~A\n" str))

  (define (clear-err!)
    (set! had-err #f))

  (define (die str)
    (err! str)
    (exit 1))
  ) ; end of module
