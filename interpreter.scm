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

  (define (assert-num op x)
    ; TODO: use call/cc to not abort the process
    (or (number? x) (die (format "Operand must be a number ~A ~A" op x))))

  (define (assert-nums op x y)
    ; TODO: use call/cc to not abort the process
    (or (and (number? x) (number? y))
        (die (format "Operands must be numbers ~A ~A ~A" x op y))))

  (define (evaluate expr)
    ; TODO: put these on the types themselves? like methods
    (cond
      ((literal? expr) (literal-value expr))
      ((grouping? expr)
       (evaluate (grouping-expression expr)))
      ((unary? expr)
       (let ((right (evaluate (unary-right expr)))
             (op (token-type (unary-operator expr))))
         (case op
           ((BANG) (not (truthy? right)))
           ((MINUS)
            (assert-num op right)
            (- right))
           (else die (format "Unknown unary op ~A" op)))))
      ((binary? expr)
       (let ((left (evaluate (binary-left expr)))
             (right (evaluate (binary-right expr)))
             (op (token-type (binary-operator expr))))
         (case op
           ((GREATER)
            (assert-nums op left right)
            (> left right))
           ((GREATER_EQUAL)
            (assert-nums op left right)
            (>= left right))
           ((LESS)
            (assert-nums op left right)
            (< left right))
           ((LESS_EQUAL)
            (assert-nums op left right)
            (<= left right))
           ((BANG_EQUAL) (not (lox-equal? left right)))
           ((EQUAL_EQUAL) (lox-equal? left right))
           ((MINUS)
            (assert-nums op left right)
            (- left right))
           ((PLUS)
            (cond
              ((and (string? left) (string? right)) (string-append left right))
              ((and (number? left) (number? right)) (+ left right))
              (else (die (format "Bad types for plus ~A" expr)))))
           ((SLASH)
            (assert-nums op left right)
            (/ left right))
           ((STAR)
            (assert-nums op left right)
            (* left right))
           (else (die (format "Unknown bin op ~A" op))))))
      (else (die (format "Unknown expr type ~A" expr)))))

  (define (interpret expr)
    (evaluate expr))
)
