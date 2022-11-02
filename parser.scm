(module parser (parse
                ; TODO: figure out a better way to export these :(
                ; maybe ditch records?
		print-stmt?
		print-stmt-value
		expr-stmt?
		expr-stmt-value
		binary?
                binary-left
                binary-right
                binary-operator
                grouping?
                grouping-expression
                literal?
                literal-value
                unary?
                unary-operator
                unary-right)

  (import scheme
          scanner
          util
          (chicken base)
          (chicken format))

  (define abort #f)

  (define-record binary left operator right)
  (set-record-printer! binary
    (lambda (x out) (fprintf out "(~A ~S ~S)"
                             (token-lexeme (binary-operator x))
                             (binary-left x)
                             (binary-right x))))

  (define-record grouping expression)
  (set-record-printer! grouping
    (lambda (x out) (fprintf out "(group ~S)" (grouping-expression x))))

  (define-record literal value)
  (set-record-printer! literal
    (lambda (x out) (fprintf out "~S" (literal-value x))))

  (define-record unary operator right)
  (set-record-printer! unary
    (lambda (x out) (fprintf out "(~A ~S)"
                             (token-lexeme (unary-operator x))
                             (unary-right x))))

  (define-record print-stmt value)
  (set-record-printer! print-stmt
		       (lambda (x out)
			 (fprintf out "(print ~A)" (print-stmt-value x))))

  (define-record expr-stmt value)
  (set-record-printer! expr-stmt
		       (lambda (x out)
			 (fprintf out "(expr ~A)" (expr-stmt-value x))))

  (define (top-type? tokens types)
    (memq (token-type (car tokens)) types))

  (define (parse tokens fname)

    (define (panic tok msg)
      (if (eq? (token-type tok) 'EOF)
        (err! (format "~A:~A:~A ~A" fname (token-line tok) "Error at end." msg))
        (err! (format "~A:~A:~A ~A. ~A"
                      fname
                      (token-line tok)
                      "Error at"
                      (token-lexeme tok)
                      msg)))
      ; TODO: synchronize instead of abort
      (abort #f))

    (define (statement tokens)
      (if (top-type? tokens '(PRINT))
	  (print-statement (cdr tokens))
	  (expression-statement tokens)))

    (define (print-statement tokens)
      (let ((ret (expression '() tokens)))
	(let ((expr (car ret)) (toks (cdr ret)))
	  (if (top-type? toks '(SEMICOLON))
	      (cons (make-print-stmt expr) (cdr toks))
	      (panic (car toks) "expected ;")))))

    (define (expression-statement tokens)
      (let ((ret (expression '() tokens)))
	(let ((expr (car ret)) (toks (cdr ret)))
	  (if (top-type? toks '(SEMICOLON))
	      (cons (make-expr-stmt expr) (cdr toks))
	      (panic (car toks) "expected ;")))))

    (define (expression expr toks)
      (equality expr toks))

    (define (equality expr toks)
      ; (print (format "equality ~S ~S" expr toks))
      (let ((ret (comparison expr toks)))
        (let loop ((e (car ret)) (ts (cdr ret)))
          (if (top-type? ts '(BANG_EQUAL EQUAL_EQUAL))
            (let ((ret2 (comparison e (cdr ts))))
              (loop (make-binary e (car ts) (car ret2)) (cdr ret2)))
            (cons e ts)))))

    (define (comparison expr toks)
      ; (print (format "comparison ~S ~S" expr toks))
      (let ((ret (term expr toks)))
        (let loop ((e (car ret)) (ts (cdr ret)))
          (if (top-type? ts '(GREATER GREATER_EQUAL LESS LESS_EQUAL))
            (let ((ret2 (term e (cdr ts))))
              (loop (make-binary e (car ts) (car ret2)) (cdr ret2)))
            (cons e ts)))))

    (define (term expr toks)
      ; (print (format "term ~S ~S" expr toks))
      (let ((ret (factor expr toks)))
        (let loop ((e (car ret)) (ts (cdr ret)))
          (if (top-type? ts '(MINUS PLUS))
            (let ((ret2 (factor e (cdr ts))))
              (loop (make-binary e (car ts) (car ret2)) (cdr ret2)))
            (cons e ts)))))

    (define (factor expr toks)
      ; (print (format "factor ~S ~S" expr toks))
      (let ((ret (unary expr toks)))
        (let loop ((e (car ret)) (ts (cdr ret)))
          (if (top-type? ts '(SLASH STAR))
            (let ((ret2 (unary e (cdr ts))))
              (loop (make-binary e (car ts) (car ret2)) (cdr ret2)))
            (cons e ts)))))

    (define (unary expr toks)
      ; (print (format "unary ~S ~S" expr toks))
      (if (top-type? toks '(BANG MINUS))
        (let ((ret (unary expr (cdr toks))))
          (cons (make-unary (car toks) (car ret)) (cdr ret)))
        (primary expr toks)))

    (define (primary expr toks)
      ; (print (format "primary ~S ~S" expr toks))
      (cond
        ((top-type? toks '(FALSE)) (cons (make-literal #f) (cdr toks)))
        ((top-type? toks '(TRUE)) (cons (make-literal #t) (cdr toks)))
        ; XXX: nil vs false?
        ((top-type? toks '(NIL)) (cons (make-literal '()) (cdr toks)))
        ((top-type? toks '(NUMBER STRING))
         (cons (make-literal (token-literal (car toks))) (cdr toks)))
        ((top-type? toks '(LEFT_PAREN))
         (let ((ret (expression expr (cdr toks))))
           (if (eq? (token-type (cadr ret)) 'RIGHT_PAREN)
             (cons (make-grouping (car ret)) (cddr ret))
             (panic (cadr ret) "Expected ')'"))))
        (else (panic (car toks) "Unknown token"))))

    ;; Actual body of parse!
    (call/cc (lambda (cc)
	       (set! abort cc)
	       (let loop ((toks tokens))
		 (if (not (top-type? toks '(EOF)))
		     (let ((ret (statement toks)))
		       (cons (car ret) (loop (cdr ret))))
		     '())))))
)
