(module interpreter (interpret)

  (import scheme
          util
          scanner
          parser
          (chicken base)
          (chicken format))

  (define (truthy? x)
    (not (or (null? x) (eq? x #f))))

  (define (lox-equal? a b)
    (cond
      ((and (null? a) (null? b)) #t)
      ((null? a) #f)
      (else (equal? a b))))

  (define (evaluate expr)
    ; TODO: put these on the types themselves? like methods
    (cond
      ((literal? expr) (literal-value expr))
      ((grouping? expr)
       (evaluate (grouping-expression expr)))
      ((unary? expr)
       (let ((right (evaluate (unary-right expr)))
             (op (token-type (unary-operator expr))))
         (cond
           ((eq? op 'BANG) (truthy? right))
           ((eq? op 'MINUS) (- right))
           ((die (format "Unknown unary op ~A" op))))))
      ((binary? expr)
       (let ((left (evaluate (binary-left expr)))
             (right (evaluate (binary-right expr)))
             (op (token-type (binary-operator expr))))
         (cond
           ((eq? op 'GREATER) (> left right))
           ((eq? op 'GREATER_EQUAL) (>= left right))
           ((eq? op 'LESS) (< left right))
           ((eq? op 'LESS_EQUAL) (<= left right))
           ((eq? op 'BANG_EQUAL) (not (lox-equal? left right)))
           ((eq? op 'EQUAL_EQUAL) (lox-equal? left right))
           ((eq? op 'MINUS) (- left right))
           ((eq? op 'PLUS)
            (cond
              ((and (string? left) (string? right)) (string-append left right))
              ((and (number? left) (number? right)) (+ left right))
              (else (die (format "Bad types for plus ~A") expr))))
           ((eq? op 'SLASH) (/ left right))
           ((eq? op 'STAR) (* left right))
           (else (die (format "Unknown bin op ~A" op))))))
      (else (die (format "Unknown expr type ~A" expr)))))

  (define (interpret expr)
    ; TODO: handle errors!
    (evaluate expr))
)
