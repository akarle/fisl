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
      (define (tok-range type s2 i2)
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

      (define (tok type)
        ; helper to tokenize current span
        (tok-range type s i))

      (define (skip)
        ; Helper to skip this character range
        (get-tokens (add1 i) (add1 i) line in))

      (define (advance l2)
        ; Helper to iterate; keeps start but increments range
        (get-tokens s (add1 i) l2 in))

      (let ((c (peek i)))
        (if (and (not in) (not c))
          (list (make-token 'EOF "" #f line))
          (cond
            ((eq? in 'comment) (if (or (not c) (eq? #\newline c))
                                 (get-tokens (add1 i) (add1 i) (add1 line) #f)
                                 (advance line)))
            ((eq? in 'string)
             (cond
               ((not c) (err (format "~A:~A:unterminated string" fname line)))
               ((eq? #\" c) (tok 'STRING))
               ((eq? #\newline c) (advance (add1 line)))
               (else (advance line))))
            ((eq? in 'number)
             (cond
               ((digit? c) (advance line))
               ((eq? #\. c) (get-tokens s (add1 i) line 'decimal))
               (else (tok-range 'NUMBER s (sub1 i)))))
            ((eq? in 'decimal)
             (cond
               ((digit? c) (advance line))
               (else (tok-range 'NUMBER s (sub1 i)))))
            ((eq? in 'alpha)
             (cond
               ((alnum? c) (advance line))
               (else (tok-range 'IDENTIFIER s (sub1 i)))))
            ((eq? in '=) (if (eq? #\= c) (tok 'EQUAL_EQUAL) (tok 'EQUAL)))
            ((eq? in '>) (if (eq? #\> c) (tok 'GREATER_EQUAL) (tok 'GREATER)))
            ((eq? in '<) (if (eq? #\< c) (tok 'LESS_EQUAL) (tok 'LESS)))
            ((eq? in '!) (if (eq? #\= c) (tok 'BANG_EQUAL) (tok 'BANG)))
            ((eq? in '/) (if (eq? #\/ c) (get-tokens s (add1 i) line 'comment) (tok 'SLASH)))
            (else (cond
                    ((eq? #\( c) (tok 'LEFT_PAREN))
                    ((eq? #\) c) (tok 'RIGHT_PAREN))
                    ((eq? #\{ c) (tok 'LEFT_BRACE))
                    ((eq? #\} c) (tok 'RIGHT_BRACE))
                    ((eq? #\, c) (tok 'COMMA))
                    ((eq? #\. c) (tok 'DOT))
                    ((eq? #\- c) (tok 'MINUS))
                    ((eq? #\+ c) (tok 'PLUS))
                    ((eq? #\; c) (tok 'SEMICOLON))
                    ((eq? #\* c) (tok 'STAR))
                    ((eq? #\! c) (get-tokens s (add1 i) line '!))
                    ((eq? #\= c) (get-tokens s (add1 i) line '=))
                    ((eq? #\< c) (get-tokens s (add1 i) line '<))
                    ((eq? #\> c) (get-tokens s (add1 i) line '>))
                    ((eq? #\/ c) (get-tokens s (add1 i) line '/))
                    ((eq? #\" c) (get-tokens s (add1 i) line 'string))
                    ((digit? c) (get-tokens s (add1 i) line 'number))
                    ((alpha? c) (get-tokens s (add1 i) line 'alpha))
                    ((eq? #\space c) (skip))
                    ((eq? #\tab c) (skip))
                    ((eq? #\newline c) (skip))
                    (else (err (format "~A:~A:unexpected character: ~A" fname 0 c)) (skip))))))))

    (get-tokens 0 0 1 #f))
  ) ; end of module
