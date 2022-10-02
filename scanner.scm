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

  (define (digit? c)
    (and c (char<=? #\0 c) (char>=? #\9 c)))

  (define (alpha? c)
    (and c
         (or
           (eq? c #\_)
           (and (char<=? #\a c) (char>=? #\z c))
           (and (char<=? #\A c) (char>=? #\Z c)))))

  (define (get-keyword k)
    (let ((kpair (assoc k '(("and"    AND)
                            ("class"  CLASS)
                            ("else"   ELSE)
                            ("false"  FALSE)
                            ("for"    FOR)
                            ("fun"    FUN)
                            ("if"     IF)
                            ("nil"    NIL)
                            ("or"     OR)
                            ("print"  PRINT)
                            ("return" RETURN)
                            ("super"  SUPER)
                            ("this"   THIS)
                            ("true"   TRUE)
                            ("var"    VAR)
                            ("while"  WHILE)))))
      (if kpair (cadr kpair) #f)))



  (define (alnum? c)
    (and c (or (alpha? c) (digit? c))))

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
        (let ((text (substring src s2 (add1 i2))))
          (let ((tok (cond
                       ((eq? type 'STRING) (make-token type text (substring src (add1 s2) i2) line))
                       ((eq? type 'NUMBER) (make-token type text (string->number text) line))
                       ((eq? type 'IDENTIFIER)
                        (let ((k (get-keyword text)))
                          (if k
                            (make-token k text #f line)
                            (make-token 'IDENTIFIER text #f line))))
                       (else (make-token type text #f line)))))
            (cons tok (get-tokens (add1 i2) (add1 i2) line #f)))))

      (define (tok-1 type)
        ; helper for length 1 tokens at current position
        (tok type s s))

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
               ((digit? c) (next line))
               ((eq? #\. c) (get-tokens s (add1 i) line 'decimal))
               (else (tok 'NUMBER s (sub1 i)))))
            ((eq? in 'decimal)
             (cond
               ((digit? c) (next line))
               (else (tok 'NUMBER s (sub1 i)))))
            ((eq? in 'alpha)
             (cond
               ((alnum? c) (next line))
               (else (tok 'IDENTIFIER s (sub1 i)))))
            ((eq? in '=) (if (eq? #\= c) (tok 'EQUAL_EQUAL s i) (tok-1 'EQUAL)))
            ((eq? in '>) (if (eq? #\> c) (tok 'GREATER_EQUAL s i) (tok-1 'GREATER)))
            ((eq? in '<) (if (eq? #\< c) (tok 'LESS_EQUAL s i) (tok-1 'LESS)))
            ((eq? in '!) (if (eq? #\= c) (tok 'BANG_EQUAL s i) (tok-1 'BANG)))
            ((eq? in '/) (if (eq? #\/ c) (get-tokens s (add1 i) line 'comment) (tok-1 'SLASH)))
            (else (cond
                    ((eq? #\( c) (tok-1 'LEFT_PAREN))
                    ((eq? #\) c) (tok-1 'RIGHT_PAREN))
                    ((eq? #\{ c) (tok-1 'LEFT_BRACE))
                    ((eq? #\} c) (tok-1 'RIGHT_BRACE))
                    ((eq? #\, c) (tok-1 'COMMA))
                    ((eq? #\. c) (tok-1 'DOT))
                    ((eq? #\- c) (tok-1 'MINUS))
                    ((eq? #\+ c) (tok-1 'PLUS))
                    ((eq? #\; c) (tok-1 'SEMICOLON))
                    ((eq? #\* c) (tok-1 'STAR))
                    ((eq? #\! c) (get-tokens s (add1 i) line '!))
                    ((eq? #\= c) (get-tokens s (add1 i) line '=))
                    ((eq? #\< c) (get-tokens s (add1 i) line '<))
                    ((eq? #\> c) (get-tokens s (add1 i) line '>))
                    ((eq? #\/ c) (get-tokens s (add1 i) line '/))
                    ((eq? #\" c) (get-tokens s (add1 i) line 'string))
                    ((digit? c) (get-tokens s (add1 i) line 'number))
                    ((alpha? c) (get-tokens s (add1 i) line 'alpha))
                    ((eq? #\space c) (get-tokens (add1 i) (add1 i) line #f))
                    ((eq? #\tab c) (get-tokens (add1 i) (add1 i) line #f))
                    ((eq? #\newline c) (get-tokens (add1 i) (add1 i) (add1 line) #f))
                    (else (err (format "~A:~A:unexpected character: ~A" fname 0 c)) #f)))))))

    (get-tokens 0 0 1 #f))
  ) ; end of module
