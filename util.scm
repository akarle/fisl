(module util (die get err! had-err clear-err!)
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

  (define (get assoc-arr key)
    ;; fetch from assoc array and error if key not found
    (let ((tup (assoc key assoc-arr)))
      (if tup
        (cadr tup)
        (error (format "bad key ~A" key)))))

  ) ; end of module
