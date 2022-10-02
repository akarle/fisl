(module util (die get err)
(import scheme
	(chicken base)
	(chicken io)
	(chicken format))

(define (err str)
  (fprintf (current-error-port) "~A\n" str))

(define (die str)
  (err str)
  (exit 1))

(define (get assoc-arr key)
  ;; fetch from assoc array and error if key not found
  (let ((tup (assoc key assoc-arr)))
    (if tup
      (cadr tup)
      (error (format "bad key ~A" key)))))

) ; end of module
