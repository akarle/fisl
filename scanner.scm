(load "util.scm")

(module scanner (scan make-token)

(import scheme
        util
        (chicken base)
        (chicken format))

(define (make-token type lexeme literal line len)
  `((type ,type)
    (lexeme ,lexeme)
    (literal ,literal)
    (line ,line)
    (len ,len)))

(define (scan src fname)
  (define (peek i)
    ; safe string-ref
    (if (< i (string-length src))
      (string-ref src i)
      #f))

  ; CI stores state as the pointer location, which works great
  ; in a language that is prepared to update state within the
  ; branches of the switch statement.
  ;
  ; Given that we're using recursion to loop over the characters,
  ; it is an easier model to conceptualize the state of the
  ; interpreter being instead what special tokens of arbitrary
  ; length we might be in (as well as the pointer).

  (define (get-tokens start line)
    ; Gets all tokens after start

    (let loop ((i start) (nline line) (in-comment #f) (in-string #f) (in-number #f) (in-identifier #f))
      (define (tok type len)
        ; helper to make a token and recurse -- todo is redefining slow? macro?
        (cons (make-token type (substring src i (+ i len)) #f nline len)
              (get-tokens (+ i len) nline)))
      (let ((c (peek i)) (n (peek (add1 i))))
        ;(printf "c: ~A, n: ~A, i: ~A, com: ~A\n" c n i in-comment)
        (if (not c)
          (list (make-token 'EOF "" #f line 0))
          (cond
            (in-comment (if (eq? #\newline c)
                          (loop (add1 i) (add1 nline) #f #f #f #f)
                          (loop (add1 i) nline #t #f #f #f)))
            (in-string #f)
            (in-number #f)
            (in-identifier #f)
            (else (cond
                    ((eq? #\( c) (tok 'LEFT_PAREN 1))
                    ((eq? #\) c) (tok 'RIGHT_PAREN 1))
                    ((eq? #\{ c) (tok 'LEFT_BRACE 1))
                    ((eq? #\} c) (tok 'RIGHT_BRACE 1))
                    ((eq? #\, c) (tok 'COMMA 1))
                    ((eq? #\. c) (tok 'DOT 1))
                    ((eq? #\- c) (tok 'MINUS 1))
                    ((eq? #\+ c) (tok 'PLUS 1))
                    ((eq? #\; c) (tok 'SEMICOLON 1))
                    ((eq? #\* c) (tok 'STAR 1))
                    ((eq? #\! c) (if (eq? #\= n) (tok 'BANG_EQUAL 2) (tok 'BANG 1)))
                    ((eq? #\= c) (if (eq? #\= n) (tok 'EQUAL_EQUAL 2) (tok 'EQUAL 1)))
                    ((eq? #\< c) (if (eq? #\< n) (tok 'LESS_EQUAL 2) (tok 'LESS 1)))
                    ((eq? #\> c) (if (eq? #\> n) (tok 'GREATER_EQUAL 2) (tok 'GREATER 1)))
                    ((eq? #\/ c) (if (eq? #\/ n) (loop (add1 i) nline #t #f #f #f) (tok 'SLASH 1)))
                    ((eq? #\space c) (loop (add1 i) nline #f #f #f #f))
                    ((eq? #\tab c) (loop (add1 i) nline #f #f #f #f))
                    ((eq? #\newline c) (loop (add1 i) (add1 nline) #f #f #f #f))
                    (else (err (format "~A:~A:unexpected character: ~A" fname 0 c)) #f))))))))

  (get-tokens 0 1))
) ; end of module
