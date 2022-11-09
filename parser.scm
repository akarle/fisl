;; parser.scm -- parser routines
(import (chicken format))

(define parser-abort #f)


;; EXPRESSIONS
(define-record binary left operator right)
(define-record grouping expression)
(define-record literal value)
(define-record unary operator right)
(define-record variable name)
(define-record assignment name value)

(set-record-printer! binary
  (lambda (x out) (fprintf out "(~A ~S ~S)"
                           (token-lexeme (binary-operator x))
                           (binary-left x)
                           (binary-right x))))

(set-record-printer! grouping
  (lambda (x out) (fprintf out "(group ~S)" (grouping-expression x))))

(set-record-printer! literal
  (lambda (x out) (fprintf out "~S" (literal-value x))))

(set-record-printer! unary
  (lambda (x out) (fprintf out "(~A ~S)"
                           (token-lexeme (unary-operator x))
                           (unary-right x))))

(set-record-printer! variable
  (lambda (x out) (fprintf out "~A" (token-lexeme (variable-name x)))))

(set-record-printer! assignment
  (lambda (x out) (fprintf out "(set! ~A ~A)" (token-lexeme (assignment-name x)) (assignment-value x))))



;; STATEMENTS
(define-record print-stmt value)
(define-record expr-stmt value)
(define-record var-stmt name init)

(set-record-printer! print-stmt
		     (lambda (x out)
		       (fprintf out "(print ~A)" (print-stmt-value x))))

(set-record-printer! expr-stmt
		     (lambda (x out)
		       (fprintf out "(expr ~A)" (expr-stmt-value x))))

(set-record-printer! var-stmt
		     (lambda (x out)
		       (fprintf out "(var ~A ~A)" (var-stmt-name x) (var-stmt-init x))))


;; helper to check if first is of types
(define (top-type? tokens types)
  (memq (token-type (car tokens)) types))


(define (parse-declaration tokens)
  (if (top-type? tokens '(VAR))
      ;; TODO: sync on failure
      (parse-var-decl (cdr tokens))
      (parse-statement tokens)))

(define (parse-var-decl tokens)
  (if (top-type? tokens '(IDENTIFIER))
      (let-values (((init toks)
                    (if (top-type? (cdr tokens) '(EQUAL))
                      (parse-expression '() (cddr tokens))
                      (values '() (cdr tokens)))))
        (if (top-type? toks '(SEMICOLON))
            (values (make-var-stmt (car tokens) init) (cdr toks))
            (parse-err! (car toks) "Expected ';' after variable declaration")))
      (parse-err! (car tokens) "expected variable name")))

(define (parse-statement tokens)
  (if (top-type? tokens '(PRINT))
      (parse-print-statement (cdr tokens))
      (parse-expression-statement tokens)))

;; Used for print and expr statements, which have the same formula
(define (parse-generic-stmt tokens maker)
  (let-values (((expr toks) (parse-expression '() tokens)))
    (if (top-type? toks '(SEMICOLON))
      (values (maker expr) (cdr toks))
      (parse-err! (car toks) "expected ;"))))

(define (parse-print-statement tokens)
  (parse-generic-stmt tokens make-print-stmt))

(define (parse-expression-statement tokens)
  (parse-generic-stmt tokens make-expr-stmt))

(define (parse-assignment expr toks)
  (let-values (((e2 t2) (parse-equality expr toks)))
    (if (top-type? t2 '(EQUAL))
      (let-values (((e3 t3) (parse-assignment e2 (cdr t2))))
        (if (variable? e2)
          (values (make-assignment (variable-name e2) e3) t3)
          (begin (err! "Invalid parse-assignment target") (values e2 t3))))
      (values e2 t2))))

(define (parse-expression expr toks)
  (parse-assignment expr toks))

;; Most of the binary operators have the same pattern:
;;   1. Evaluate the left side of the expression
;;   2. While the top is the operator, keep evaluating / building up the expression
;;   3. Return once the operator isn't matched
;; This function does it all, with a generic 'lower' to evaluate if 'types' matched
(define (parse-generic-binary expr tokens lower types)
  (let-values (((e2 t2) (lower expr tokens)))
    (let loop ((e e2) (ts t2))
      (if (top-type? ts types)
        ;; top of ts is an operator, eval right side on rest
        (let-values (((e3 t3) (lower e (cdr ts))))
          (loop (make-binary e (car ts) e3) t3))
        (values e ts)))))

(define (parse-equality expr toks)
  (parse-generic-binary expr toks parse-comparison '(BANG_EQUAL EQUAL_EQUAL)))

(define (parse-comparison expr toks)
  (parse-generic-binary expr toks parse-term '(GREATER GREATER_EQUAL LESS LESS_EQUAL)))

(define (parse-term expr toks)
  (parse-generic-binary expr toks parse-factor '(MINUS PLUS)))

(define (parse-factor expr toks)
  (parse-generic-binary expr toks parse-unary '(SLASH STAR)))

(define (parse-unary expr toks)
  (if (top-type? toks '(BANG MINUS))
      (let-values (((e2 t2) (parse-unary expr (cdr toks))))
        (values (make-unary (car toks) e2) t2))
      (parse-primary expr toks)))

(define (parse-primary expr toks)
  (let ((top (car toks)) (rest (cdr toks)))
    (cond
     ((top-type? toks '(FALSE)) (values (make-literal #f) rest))
     ((top-type? toks '(TRUE)) (values (make-literal #t) rest))
     ((top-type? toks '(NIL)) (values (make-literal '()) rest))
     ((top-type? toks '(NUMBER STRING))
      (values (make-literal (token-literal top)) rest))
     ((top-type? toks '(IDENTIFIER)) (values (make-variable top) rest))
     ((top-type? toks '(LEFT_PAREN))
      (let-values (((e2 t2) (parse-expression expr rest)))
        (if (top-type? t2 '(RIGHT_PAREN))
            (values (make-grouping e2) (cdr t2))
            (parse-err! (car t2) "Expected ')'"))))
     (else (parse-err! (car toks) "Unknown token")))))

(define (parse-err! tok msg)
  (if (eq? (token-type tok) 'EOF)
      (fname-err! (format "~A:~A ~A" (token-line tok) "Error at end." msg))
      (fname-err! (format "~A:~A ~A. ~A"
                    (token-line tok)
                    "Error at"
                    (token-lexeme tok)
                    msg)))
  ;; TODO: synchronize instead of abort
  (parser-abort #f))

(define (parse tokens)
  (call/cc (lambda (cc)
	     (set! parser-abort cc)
	     (let loop ((toks tokens))
	       (if (not (top-type? toks '(EOF)))
		   (let-values (((expr rest) (parse-declaration toks)))
		     (cons expr (loop rest)))
		   '())))))
