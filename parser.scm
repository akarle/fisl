(module parser (parse)

  (import scheme
          (chicken base)
          (chicken format))

  (define-record binary left operator right)
  (set-record-printer! binary
    (lambda (x out) (fprintf out "(~S ~S ~S)" (binary-operator x) (binary-left x) (binary-right x))))

  (define-record grouping expression)
  (set-record-printer! grouping
    (lambda (x out) (fprintf out "(group ~S)" (grouping-expression x))))

  (define-record literal value)
  (set-record-printer! literal
    (lambda (x out) (fprintf out "~S" (literal-value x))))

  (define-record unary operator right)
  (set-record-printer! unary
    (lambda (x out) (fprintf out "(~S ~S)" (unary-operator x) (unary-right x))))

  (define (parse)
    (print (make-binary (make-unary "-" (make-literal 123)) "*" (make-grouping (make-literal 42)))))
)
