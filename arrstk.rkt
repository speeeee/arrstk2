#lang racket/base
(require racket/list)

; successor to arrstk, this language will look like any other stack language
; for a little bit.

(struct v (val type))
(struct fn (name ins))
(define (push stk elt) (append stk (list elt)))
(define (pop stk) (car (reverse stk)))
(define (ret-pop stk) (reverse (cdr (reverse stk))))

(define funs* (list (list "+" (list "#Int" "#Int") (list "#Int"))
                    (list "(define)" (list "#Set" "#List" "#List" "#Sym") '()) (list "(lst)")
                    (list ";")))
;(define macros* (list (list ":IN") (list ":OUT")))

(define (write-spec ls) 
  (if (list? ls) (begin (display "(") (map write-spec ls) (display ")"))
      (cond [(v? ls) (begin (display "(v ") (write-spec (v-val ls)) (write-spec (v-type ls)) (display ")"))] 
            [(fn? ls) (begin (display "(f ") (write-spec (fn-name ls)) (write-spec (fn-ins ls)) (display ")"))]
            [else (write ls)])))

(define (string-split-spec str)
  (filter (λ (x) (not (empty? (string->list x)))) (splt str '())))
  ;(splt str '()))
(define (splt str lst)
  (if (empty? (string->list str)) lst
      (splt (cadr (tok (string->list str) '())) (append lst (list (car (tok (string->list str) '())))))))

(define (tok str lst)
  (if (empty? str) (list (list->string lst) "")
    (let ([c (car str)])
      (if (and (not (empty? lst)) (equal? (car lst) #\"))
          (if (equal? c #\") (list (list->string (append lst (list c))) (list->string (cdr str)))
              (tok (cdr str) (append lst (list c))))
          (if (or (char-whitespace? c)) (if (empty? lst) (tok (cdr str) lst) (list (list->string lst) (list->string str)))
              (tok (cdr str) (append lst (list c))))))))

(define (strcar s) (car (string->list s)))

(define (out-c stk f) ;(displayln stk)
  (cond [(list? stk) (begin (map (λ (x) (begin (out-c x f) (fprintf f ";~n"))) stk))]
        [(fn? stk) (begin (fprintf f "~a(" (fn-name stk))
                          (map (λ (x) (begin (out-c x f) (fprintf f ","))) (ret-pop (fn-ins stk))) (out-c (pop (fn-ins stk)) f)
                          (fprintf f ")"))]
        [(v? stk) (out-c (v-val stk) f)]
        [else (fprintf f "~a" stk)]))
        

(define (lex l)
  (cond [(or (char-numeric? (strcar l)) (char=? (strcar l) #\.)) (v l "#Int")]
        [(char=? (strcar l) #\") (v l "#String")]
        [(char=? (strcar l) #\#) (v l "#Type")]
        [else (v l "#Sym")]))

(define (fexists? s fns) (member s (map car fns)))
(define (get-f s fns) (findf (λ (x) (equal? (car x) s)) fns))
(define (call-fun f stk)
  (cond [(equal? (car f) "(lst)") (let ([l (λ (x) (equal? (v-type (car (reverse stk))) (v-type x)))])
                                    (push (reverse (dropf (reverse stk) l)) (v (reverse (takef (reverse stk) l)) "#List")))]
        [(equal? (car f) "(define)") (let ([x (pop stk)] [y (pop (ret-pop stk))] [z (pop (ret-pop (ret-pop stk)))])
                                       (set! funs* (push funs* (list (v-val x) (map v-val (v-val z)) (map v-val (v-val y))))))]
        [(equal? (car f) ";") (list (v stk "#Set"))]
        [else 
  (let ([sub #;(list (pop (ret-pop stk)) (pop stk)) (drop stk (- (length stk) (length (second f))))])
    (if (not (equal? (map v-type sub) (second f))) (begin (displayln (map v-type sub)) (displayln "ERROR: type mismatch."))
        (begin ; (do the C stuff)
               (append (take stk (- (length stk) (length sub))) (map (λ (x) (v (fn (car f) sub) x)) (third f))))))]))

(define (push~ stk s)
  (cond [(equal? (v-type s) "#Sym") (if (fexists? (v-val s) funs*) (call-fun (get-f (v-val s) funs*) stk)
                                        (push stk s))]
        [else (push stk s)]))

(define (process stk n)
  (if (empty? stk) n (process (cdr stk) (push~ n (car stk)))))

(define (main)
  (let ([e (process (map lex (string-split-spec (read-line))) '())])
    (write-spec e) (out-c e (current-output-port))
    (displayln funs*)
    (main)))

(main)