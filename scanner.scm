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
  (define (comment i)
    ; parse comment until end, return stopping point
    (let loop ((curr i))
      (if (and (< curr (string-length src))
               (not (eq? #\newline (string-ref src curr))))
        (loop (add1 curr))
        (- curr i))))

  (define (peek i)
    ; safe string-ref
    (if (< i (string-length src))
      (string-ref src i)
      'nil))

  (define (loop i line)
    (define (tok type len)
      (make-token type (substring src i (+ i len)) 'nil line len))
    (if (< i (string-length src))
      (begin
        (let ((c (string-ref src i)) (n (peek (add1 i))))
          (let ((tok (cond
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
            ((eq? #\/ c) (if (eq? #\/ n) (tok 'COMMENT (comment i)) (tok 'SLASH 1)))
            ((eq? #\space c) #f)
            ((eq? #\tab c) #f)
            ((eq? #\newline c) #f)
            ;; TODO: set/return hadError (keep scanning)
            (else (err (format "~A:~A:unexpected character: ~A" fname line c)) #f))))
            (if tok
              (begin (print tok)
                     (loop (+ i (get tok 'len)) line))
              (loop (add1 i) line)))))
    'EOF))
  (loop 0 1))

) ; end of module
