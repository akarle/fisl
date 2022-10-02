(load "util.scm")

(module scanner (scan)

(import scheme
        util
        (chicken base)
        (chicken format))

(define (make-token type lexeme literal line)
  `((type ,type)
    (lexeme ,lexeme)
    (literal ,literal)
    (line ,line)))

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



  (define (get-tokens s i line in)
    ; Gets all tokens after 'start', tracks state in i (current char), line, in
    (define (tok type s2 i2)
      ; Helper to make a token, cons it to our list, and recurse with fresh state
      (cons (make-token type (substring src s2 (add1 i2)) #f line)
            (get-tokens (add1 i2) (add1 i2) line #f)))

    (let ((c (peek i)))
      (if (not c)
        (list (make-token 'EOF "" #f line))
        (cond
          ((eq? in 'comment) (if (eq? #\newline c)
                                 (get-tokens (add1 i) (add1 i) (add1 line) #f)
                                 (get-tokens s (add1 i) line 'comment)))
          ((eq? in 'string) #f)
          ((eq? in 'number) #f)
          ((eq? in 'identifier) #f)
          ((eq? in '=) (if (eq? #\= c) (tok 'EQUAL_EQUAL s i) (tok 'EQUAL s s)))
          ((eq? in '>) (if (eq? #\> c) (tok 'GREATER_EQUAL s i) (tok 'GREATER s s)))
          ((eq? in '<) (if (eq? #\< c) (tok 'LESS_EQUAL s i) (tok 'LESS s s)))
          ((eq? in '!) (if (eq? #\= c) (tok 'BANG_EQUAL s i) (tok 'BANG s s)))
          ((eq? in '/) (if (eq? #\/ c) (get-tokens s (add1 i) line 'comment) (tok 'SLASH s s)))
          (else (cond
                  ((eq? #\( c) (tok 'LEFT_PAREN s s))
                  ((eq? #\) c) (tok 'RIGHT_PAREN s s))
                  ((eq? #\{ c) (tok 'LEFT_BRACE s s))
                  ((eq? #\} c) (tok 'RIGHT_BRACE s s))
                  ((eq? #\, c) (tok 'COMMA s s))
                  ((eq? #\. c) (tok 'DOT s s))
                  ((eq? #\- c) (tok 'MINUS s s))
                  ((eq? #\+ c) (tok 'PLUS s s))
                  ((eq? #\; c) (tok 'SEMICOLON s s))
                  ((eq? #\* c) (tok 'STAR s s))
                  ((eq? #\! c) (get-tokens s (add1 i) line '!))
                  ((eq? #\= c) (get-tokens s (add1 i) line '=))
                  ((eq? #\< c) (get-tokens s (add1 i) line '<))
                  ((eq? #\> c) (get-tokens s (add1 i) line '>))
                  ((eq? #\/ c) (get-tokens s (add1 i) line '/))
                  ((eq? #\space c) (get-tokens (add1 i) (add1 i) line #f))
                  ((eq? #\tab c) (get-tokens (add1 i) (add1 i) line #f))
                  ((eq? #\newline c) (get-tokens (add1 i) (add1 i) (add1 line) #f))
                  (else (err (format "~A:~A:unexpected character: ~A" fname 0 c)) #f)))))))

  (get-tokens 0 0 1 #f))
) ; end of module
