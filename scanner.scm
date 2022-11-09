;; scanner.scm -- tokenizes input
(import (chicken format))

;; Auto-generates the scaffolding getters and setters
;;   make-token, token-type, set-token-type!, etc
(define-record token type lexeme literal line)
(set-record-printer! token (lambda (t out)
			     (fprintf out "#,(token type:~S lex:~S lit:~S ln:~S)"
				      (token-type t) (token-lexeme t) (token-literal t) (token-line t))))

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

(define (scan src)
  (define (peek i)
    ;; safe string-ref
    (if (< i (string-length src))
        (string-ref src i)
        #f))

  (define (get-tokens s i line in)
    ;; Gets all tokens after 'start', tracks state in i (current char), line, in
    (define (tok-range type s2 i2)
      ;; Helper to make a token, cons it to our list, and recurse with fresh state
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
      ;; helper to tokenize current span
      (tok-range type s i))

    (define (skip . line2)
      ;; Helper to skip this character range
      (get-tokens (add1 i) (add1 i) (optional line2 line) in))

    (define (advance . line2)
      ;; Helper to iterate; keeps start but increments range
      (get-tokens s (add1 i) (optional line2 line) in))

    (let ((c (peek i)) (n (peek (add1 i))))
      (if (and (not in) (not c))
          (list (make-token 'EOF "" #f line))
          (cond
           ((eq? in 'comment) (if (or (not c) (eq? #\newline c))
                                  (get-tokens (add1 i) (add1 i) (add1 line) #f)
                                  (advance)))
           ((eq? in 'string)
            (cond
             ((not c) (fname-err! (format "~A:unterminated string" line)))
             ((eq? #\" c) (tok 'STRING))
             ((eq? #\newline c) (advance (add1 line)))
             (else (advance))))
           ((eq? in 'number)
            (cond
             ((digit? c) (advance))
             ((eq? #\. c) (get-tokens s (add1 i) line 'decimal))
             (else (tok-range 'NUMBER s (sub1 i)))))
           ((eq? in 'decimal)
            (cond
             ((digit? c) (advance))
             (else (tok-range 'NUMBER s (sub1 i)))))
           ((eq? in 'alpha)
            (cond
             ((alnum? c) (advance))
             (else (tok-range 'IDENTIFIER s (sub1 i)))))
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
                  ((eq? #\! c) (if (eq? #\= n) (tok-range 'BANG_EQUAL s (add1 i)) (tok 'BANG)))
                  ((eq? #\= c) (if (eq? #\= n) (tok-range 'EQUAL_EQUAL s (add1 i) ) (tok 'EQUAL)))
                  ((eq? #\< c) (if (eq? #\= n) (tok-range 'LESS_EQUAL s (add1 i) ) (tok 'LESS)))
                  ((eq? #\> c) (if (eq? #\= n) (tok-range 'GREATER_EQUAL s (add1 i) ) (tok 'GREATER)))
                  ((eq? #\/ c) (if (eq? #\/ n) (get-tokens s (add1 i) line 'comment) (tok 'SLASH)))
                  ((eq? #\" c) (get-tokens s (add1 i) line 'string))
                  ((digit? c) (get-tokens s (add1 i) line 'number))
                  ((alpha? c) (get-tokens s (add1 i) line 'alpha))
                  ((eq? #\space c) (skip))
                  ((eq? #\tab c) (skip))
                  ((eq? #\newline c) (skip (add1 line)))
                  (else (fname-err! (format "~A:unexpected character: ~A" line c)) (skip))))))))

  (get-tokens 0 0 1 #f))
