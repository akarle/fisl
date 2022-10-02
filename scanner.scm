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

(define (is-digit c)
  (and (char<=? #\0 c) (char>=? #\9 c)))

(define (scan src fname)
  (define (peek i)
    ; safe string-ref
    (if (< i (string-length src))
      (string-ref src i)
      #f))

  (define (get-tokens s i line in)
    ; Gets all tokens after 'start', tracks state in i (current char), line, in
    (define (tok type s2 i2)
      ; Helper to make a token, cons it to our list, and recurse with fresh state
      (let ((tok (cond
                  ((eq? type 'STRING) (make-token type (substring src (add1 s2) i2) #f line))
                  ((eq? type 'NUMBER) (make-token type
                                                  (string->number (substring src s2 (add1 i2)))
                                                  #f line))
                  (else (make-token type (substring src s2 (add1 i2)) #f line)))))
        (cons tok (get-tokens (add1 i2) (add1 i2) line #f))))

    (define (next l2)
      ; Helper to iterate while keeping state
      (get-tokens s (add1 i) l2 in))

    (let ((c (peek i)))
      (if (and (not in) (not c))
        (list (make-token 'EOF "" #f line))
        (cond
          ((eq? in 'comment) (if (or (not c) (eq? #\newline c))
                                 (get-tokens (add1 i) (add1 i) (add1 line) #f)
                                 (next line)))
          ((eq? in 'string)
           (cond
             ((not c) (err (format "~A:~A:unterminated string" fname line)))
             ((eq? #\" c) (tok 'STRING s i))
             ((eq? #\newline c) (next (add1 line)))
             (else (next line))))
          ((eq? in 'number)
           (cond
             ((is-digit c) (next line))
             ((eq? #\. c) (get-tokens s (add1 i) line 'decimal))
             (else (tok 'NUMBER s (sub1 i)))))
          ((eq? in 'decimal)
           (cond
             ((is-digit c) (next line))
             (else (tok 'NUMBER s (sub1 i)))))
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
                  ((eq? #\" c) (get-tokens s (add1 i) line 'string))
                  ((is-digit c) (get-tokens s (add1 i) line 'number))
                  ((eq? #\space c) (get-tokens (add1 i) (add1 i) line #f))
                  ((eq? #\tab c) (get-tokens (add1 i) (add1 i) line #f))
                  ((eq? #\newline c) (get-tokens (add1 i) (add1 i) (add1 line) #f))
                  (else (err (format "~A:~A:unexpected character: ~A" fname 0 c)) #f)))))))

  (get-tokens 0 0 1 #f))
) ; end of module
