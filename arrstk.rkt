#lang racket/base
(require racket/list)

; successor to arrstk, this language will look like any other stack language
; for a little bit.

(struct v (val type))
(struct fn (name ins))
(struct rule (in out))
(define (rules m) (second (v-val m)))
(define (v=? va vb) (and (equal? (v-val va) (v-val vb)) (equal? (v-type va) (v-type vb))))
(define (push stk elt) (append stk (list elt)))
(define (pop stk) (car (reverse stk)))
(define (ret-pop stk) (reverse (cdr (reverse stk))))

(define funs* (list (list "+" (list "#Int" "#Int") (list "#Int"))
                    (list "(define)" (list "#Expr" "#List" "#List" "#Sym") '()) (list "(lst)")
                    (list "(rule)") #| #Expr #List |# 
                    (list "(mode)") #| #Sym |#
                    (list "(swap)") (list "(drop)") (list "(dup)") (list "(type)") (list "(exec)")
                    (list "(add-rule)") (list "(mode-expr)") (list "(push-mode)")
                    #;(list ";")))
(define modes* '())

;(define macros* (list (m ":" (list "name" "ea" "eb" "def") 
;                         (fn "define" (list (v "name" "#Sym") (v "ea" "#List") (v "eb" "#List") (v "def" "#Expr")) "#Void"))))
; very rough sketch on how macros could work:
; they work by having a "mode" symbol, or 'trigger'.  The mode essentially
; determines how many inputs it will wait for before activating.
; there will be a way to break from the macro call to parse using a different mode.
; the words, '{' and '}', will be used to break.

;(define modes* (mode "MODE:" (list "name")
;                     (rule (list "#Rule" 

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
        [(equal? (car f) "(rule)") (let ([x (pop stk)] [y (pop (ret-pop stk))])
                                     (push (ret-pop (ret-pop stk)) (v (list (v-val y) (v-val x)) "#Rule")))]
        [(equal? (car f) "(mode)") (push (ret-pop stk) (v (list (v-val (pop stk)) '()) "#Mode"))]
        [(equal? (car f) "(add-rule)") (push (ret-pop (ret-pop stk)) 
                                               (v (list (car (v-val (pop (ret-pop stk)))) (push (second (v-val (pop (ret-pop stk))))
                                                                                                (pop stk))) "#Mode"))]
        [(equal? (car f) "(dup)") (append (ret-pop stk) (list (pop stk) (pop stk)))]
        [(equal? (car f) "(swap)") (append (ret-pop (ret-pop stk)) (list (pop stk) (pop (ret-pop stk))))]
        [(equal? (car f) "(drop)") (ret-pop stk)]
        [(equal? (car f) "(type)") (push (ret-pop (ret-pop stk)) (v (pop (ret-pop stk)) (v-val (pop stk))))]
        #;[(equal? (car f) "(mode-expr)") 
         ()]
        [(equal? (car f) "(exec)") (append (ret-pop stk) (process (check-semi (v-val (pop stk))) '()))]
        [(equal? (car f) "(push-mode)") (set! modes* (push modes* (pop stk)))]
        ;[(equal? (car f) ";") (list (v stk "#Set"))]
        [else 
  (let ([sub #;(list (pop (ret-pop stk)) (pop stk)) (drop stk (- (length stk) (length (second f))))])
    (if (not (equal? (map v-type sub) (second f))) (begin (displayln (map v-type sub)) (displayln "ERROR: type mismatch."))
        (begin ; (do the C stuff)
               (append (take stk (- (length stk) (length sub))) (map (λ (x) (v (fn (car f) sub) x)) (third f))))))]))

(define (push~ stk s)
  (cond [(equal? (v-type s) "#Sym") (if (fexists? (v-val s) funs*) (call-fun (get-f (v-val s) funs*) stk)
                                        (push stk s))]
        [else (push stk s)]))

(define (check-semi stk) (check-semi+ stk '()))
(define (check-semi+ stk n) 
  (if (empty? stk) n (cond [(v=? (car stk) (v "}" "#Sym")) (let ([l (λ (x) (not (equal? (v-val x) "{")))])
                            (check-semi+ (cdr stk) (push (ret-pop (reverse (dropf (reverse n) l))) (v (reverse (takef (reverse n) l)) "#Expr"))))]
                           [else (check-semi+ (cdr stk) (push n (car stk)))])))

(define (process stk n)
  (if (empty? stk) n (process (cdr stk) (push~ n (car stk)))))

(define (main)
  (let ([e (process (check-semi (map lex (string-split-spec (read-line)))) '())])
    (write-spec e) (out-c e (current-output-port))
    (displayln funs*)
    (main)))

(main)