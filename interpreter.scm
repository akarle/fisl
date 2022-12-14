;; interpreter.scm -- evaluates parsed statements
(import
  srfi-69 ; hash-tables
  (chicken format))

(define interpreter-abort #f)

(define (make-env parent)
  (let ((ht (make-hash-table)))
    (lambda (action)
      (cond ((eq? action 'get)
	     (lambda (el)
	       (if (hash-table-exists? ht el)
		   (hash-table-ref ht el)
		   (if parent
		       (env-get parent el)
		       (runtime-err! (format "Unbound variable ~A" el))))))
	    ((eq? action 'def)
	     ;; var block, sets in current env
	     (lambda (el val)
	       (hash-table-set! ht el val)))
	    ((eq? action 'set)
	     (lambda (el val)
	       (if (hash-table-exists? ht el)
		   (hash-table-set! ht el val)
		   (if parent
		       (env-set! parent el val)
		       (runtime-err! (format "Unable to set unbound variable ~A" el))))))
	    ((eq? action 'exists)
	     (lambda (el)
	       (if (hash-table-exists? ht el)
		   #t
		   (and parent (env-exists? parent el)))))
	    (else (error (format "Unknown action for env -- ~A" action)))))))

(define (env-get env key)
  ((env 'get) key))

(define (env-set! env key val)
  ((env 'set) key val))

(define (env-def! env key val)
  ((env 'def) key val))

(define (env-exists? env key)
  ((env 'exists) key))

(define (runtime-err! msg)
  (err! msg)
  (interpreter-abort #f))

(define (truthy? x)
  (not (or (null? x) (eq? x #f))))

(define (lox-equal? a b)
  (cond
   ((and (null? a) (null? b)) #t)
   ((null? a) #f)
   (else (equal? a b))))

(define (assert-num op x)
  (or (number? x) (runtime-err! (format "Operand must be a number ~A ~A" op x))))

(define (assert-nums op x y)
  (or (and (number? x) (number? y))
      (runtime-err! (format "Operands must be numbers ~A ~A ~A" x op y))))

(define (evaluate expr env)
  (cond
   ((literal? expr) (literal-value expr))
   ((grouping? expr)
    (evaluate (grouping-expression expr) env))
   ((variable? expr)
    (let ((tok (variable-name expr)))
      (env-get env (token-lexeme tok))))
   ((assignment? expr)
    (let ((tok (assignment-name expr)))
      (if (env-exists? env (token-lexeme tok))
        (let ((res (evaluate (assignment-value expr) env)))
          (env-set! env (token-lexeme tok) res)
          res)
        (runtime-err! (format "Unbound variable ~A at line ~A"
                              (token-lexeme tok) (token-line tok))))))
   ((unary? expr)
    (let ((right (evaluate (unary-right expr) env))
          (op (token-type (unary-operator expr))))
      (case op
        ((BANG) (not (truthy? right)))
        ((MINUS)
         (assert-num op right)
         (- right))
        (else (runtime-err! (format "Unknown unary op ~A" op))))))
   ((logical? expr)
    (let ((left (evaluate (logical-left expr) env))
          (op (token-type (logical-operator expr))))
      (case op
	((OR)
	 (if (truthy? left)
	     left
	     (evaluate (logical-right expr) env)))
	((AND)
	 (if (truthy? left)
	     (evaluate (logical-right expr) env)
	     left)))))
   ((binary? expr)
    (let ((left (evaluate (binary-left expr) env))
          (right (evaluate (binary-right expr) env))
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
          (else (runtime-err! (format "Bad types for plus ~A" expr)))))
        ((SLASH)
         (assert-nums op left right)
         (/ left right))
        ((STAR)
         (assert-nums op left right)
         (* left right))
        (else (runtime-err! (format "Unknown bin op ~A" op))))))
   (else (runtime-err! (format "Unknown expr type ~A" expr)))))

(define (lox-print val)
  (print (cond
           ((null? val) "nil")
           ((eq? val #f) "false")
           ((eq? val #t) "true")
           (else val))))

(define (execute stmt env)
  (cond
   ((print-stmt? stmt)
    (let ((res (evaluate (print-stmt-value stmt) env)))
      (lox-print res)
      '()))
   ((var-stmt? stmt)
    (let ((value
            (if (null? (var-stmt-init stmt))
              '()
              (evaluate (var-stmt-init stmt) env))))
      (env-def! env (token-lexeme (var-stmt-name stmt)) value))
    '())
   ((expr-stmt? stmt)
    (let ((res (evaluate (expr-stmt-value stmt) env)))
      (if in-repl (lox-print res))
      '()))
   ((block? stmt)
    (let ((new-env (make-env env)))
      (let loop ((stmts (block-stmts stmt)))
	(if (null? stmts)
	    '()  ; TODO: Why are we still returning null from all these?
	    (begin
	      (execute (car stmts) new-env)
	      (loop (cdr stmts)))))))
   ((if-stmt? stmt)
    (if (truthy? (evaluate (if-stmt-cond-expr stmt) env))
	(execute (if-stmt-then-stmt stmt) env)
	(if (not (null? (if-stmt-else-stmt stmt)))
	    (execute (if-stmt-else-stmt stmt) env)
	    '())))
   ((while-stmt? stmt)
    (let loop ()
      (if (truthy? (evaluate (while-stmt-cond-expr stmt) env))
	  (begin
	    (execute (while-stmt-body-stmt stmt) env)
	    (loop))
	  '())))
   (else (runtime-err! (format "Unknown stmt ~A" stmt)))))

;; Save the global-env outside interpret so that it persists in the REPL
(define global-env (make-env #f))

(define (interpret stmts)
  (call/cc (lambda (cc)
	     (set! interpreter-abort cc)
	     (let loop ((sts stmts))
	       (if (not (null? sts))
		   (begin (execute (car sts) global-env)
			  (loop (cdr sts))))))))
