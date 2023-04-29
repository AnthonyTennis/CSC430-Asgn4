#lang typed/racket

;Authors: Anthony Teciorowski, Scott Hufschmidt
; We were able to write all the necessary functions, however could not get to
; writing the recursive rounding function and passing your test cases.

(require typed/rackunit)

(struct FundefC ([name : Sexp] [args : (Listof Sexp)] [body : ExprC])
  #:transparent)
(define-type ExprC (U NumC IdC AppC BinopC leq0 FundefC))
(struct NumC ([n : Real])
   #:transparent)
(struct IdC ([s : Symbol])
   #:transparent)
(struct AppC ([f : ExprC] [args : (Listof ExprC)])
   #:transparent)
(struct BinopC ([op : binop] [l : ExprC] [r : ExprC])
   #:transparent)
(struct leq0 ([test : ExprC] [then : ExprC] [else : ExprC])
   #:transparent)
(define-type binop (U '+ '- '* '/))

; Determines if symbol is a valid operator
(define (binop? [v : Sexp]) : Boolean
  (if (symbol? v)
      (match v
        ['+ #t]
        ['- #t]
        ['* #t]
        ['/ #t]
        [_ #f])
      #f))

; test cases for binop?
(check-equal? (binop? '+) #t)
(check-equal? (binop? '-) #t)
(check-equal? (binop? '*) #t)
(check-equal? (binop? '/) #t)
(check-equal? (binop? 's) #f)
(check-equal? (binop? 1) #f)



; ---------------------------------------------------
; Parser and tests

; This function takes a list of S-expressions as input and returns a list of ExprC
(define (parse-args [args : (Listof Sexp)]) : (Listof ExprC)
  (if (null? args)
      '()
      (cons (parse (car args)) (parse-args (cdr args)))))

; the parse function will take in an s-expression and modify it to create an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(list 'leq0? test 'then then 'else else)
     (leq0 (parse test) (parse then) (parse else))]
    [(list op arg1 arg2)
     (cond
       [(binop? op)
        (if (and (not (binop? arg1)) (not (binop? arg2))
                 (not (equal? arg1 'def)) (not (equal? arg2 'def)))
            (BinopC (cast op binop) (parse arg1) (parse arg2))
            (error 'parse "VVQS invalid operation ~e" s))]
       [else (AppC (parse op) (parse-args (list arg1 arg2)))])]
    [(? list? s) (let ([f (car s)]
                    [args (parse-args (cdr s))])
                   (if (binop? f) (error 'parse "VVQS invalid operation ~e" s)
                       (AppC (parse f) args)))]
    [(? real? n) (NumC n)]
    [(? symbol? s)
     (if (or (equal? s 'def) (equal? s 'leq0?))
         (error 'parse "VVQS forbidden symbol usage")
         (IdC s))]
    [_ (error 'parse "VVQS invalid input ~e" s)]))

; test cases for exprc-equal? and parse
(check-equal? (parse '(+ 1 2)) (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (parse '(* 3 4)) (BinopC '* (NumC 3) (NumC 4)))
(check-equal? (parse '(/ 10 5)) (BinopC '/ (NumC 10) (NumC 5)))
(check-equal? (parse '(f 12)) (AppC (IdC 'f) (list (NumC 12))))
(check-exn (regexp (regexp-quote "VVQS invalid operation"))
           (lambda () (parse '(/ 3 4 5))))
(check-exn (regexp (regexp-quote "VVQS invalid input"))
           (lambda () (parse #t)))
(check-exn (regexp (regexp-quote "VVQS invalid operation"))
           (lambda () (parse '(+ / 3))))
(check-exn (regexp (regexp-quote "VVQS forbidden symbol usage"))
           (lambda () (parse 'leq0?)))
(check-exn (regexp (regexp-quote "VVQS invalid operation"))
           (lambda () (parse '(+ def a))))
(check-exn (regexp (regexp-quote "VVQS forbidden symbol usage"))
           (lambda () (parse 'def)))
; Last thing to check on
;(check-exn (regexp (regexp-quote "VVQS forbidden symbol usage"))
;           (lambda () (parse '*)))

; ---------------------------------------------------
; Interpreter and tests

; Executes a BinopC (or Num) statement and returns a real
(define (binop-exec [b : ExprC]) : Real
  (match b
    [(NumC n) n]
    [(BinopC '+ l r) (+ (binop-exec l) (binop-exec r))]
    [(BinopC '- l r) (- (binop-exec l) (binop-exec r))]
    [(BinopC '* l r) (* (binop-exec l) (binop-exec r))]
    [(BinopC '/ l r)
     (if (equal? (binop-exec r) 0)
         (error 'binop-exec "VVQS Division by zero ~e" b)
         (/ (binop-exec l) (binop-exec r)))]))

; Takes in an ExprC and evaluates it
(define (interp [e : Any] [funs : (Listof FundefC)]) : Real
  (match e
    [(NumC n) n]
    [(IdC s) (error 'interp "VVQS incorrect identifier: ~a" s)]
    [(BinopC op e1 e2)
     (if (and (NumC? e1) (NumC? e2))
         (binop-exec e)
         (binop-exec (BinopC op (NumC (interp e1 funs)) (NumC (interp e2 funs)))))]
    [(leq0 test then else)
     (if (<= (interp test funs) 0)
         (interp then funs)
         (interp else funs))]
    [(AppC f args)
     (define f-fn (find-function f funs))
     (match f-fn
       [(FundefC n formals body)
        (if (= (length formals) (length args))
            (interp (subst-all formals (map (λ (e) (NumC (interp e funs))) args) body) funs)
            (error 'interp "VVQS argument mismatch: ~a" f))]
       [_ (error 'interp "VVQS function not found: ~a" f)])]
    [_ (error 'interp "VVQS invalid input: ~a" e)]))


(check-equal? (interp (NumC 2) '()) 2)
(check-equal? (interp (parse '(+ 1 2)) '()) 3)
(check-equal? (interp (parse '(* 3 4)) '()) 12)
(check-equal? (interp (parse '(- 2 1)) '()) 1)
(check-equal? (interp (parse '(/ 3 3)) '()) 1)
(check-exn (regexp (regexp-quote "VVQS incorrect identifier:"))
           (lambda () (interp (IdC 'a) '())))

(check-exn (regexp (regexp-quote "VVQS invalid input:"))
           (lambda () (interp '() '())))

; ---------------------------------------------------

; Parses a given function definition
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'def (list fun-name args ...) '= body)
     (FundefC fun-name args (parse body))]
    [_ (error 'parse-fundef "VVQS invalid input ~e" s)]))


; Test cases for parse-fundef
(check-equal? (parse-fundef '{def {add x y} = {+ x y}}) (FundefC 'add '(x y) (BinopC '+ (IdC 'x) (IdC 'y))))
(check-exn (regexp (regexp-quote "VVQS invalid input"))
           (lambda () (parse-fundef 'a)))

; ---------------------------------------------------

; Parses a list of s-expressions and returns a list of FundefC
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    [(? list? ls)
     (if (andmap list? ls)
         (map parse-fundef ls)
         (error 'parse-prog "VVQS invalid input function ~e" s))]
    [_ (error 'parse-prog "VVQS invalid input type ~e" s)]
    ))


; Test cases for parse-prog

(check-equal? (parse-prog '{{def {f x} = {+ x 14}}})
              (list (FundefC 'f (list (quote x)) (BinopC '+ (IdC (quote x)) (NumC 14)))))
(check-equal? (parse-prog '{{def {f x} = {+ x 14}}
                             {def {main init} = {f 2}}})
              (list (FundefC 'f (list (quote x)) (BinopC '+ (IdC (quote x)) (NumC 14)))
                    (FundefC 'main (list (quote init)) (AppC (IdC 'f) (list (NumC 2))))))
(check-equal? (parse-prog '()) '())
(check-exn (regexp (regexp-quote "VVQS invalid input type"))
           (lambda () (parse-prog 'a)))
(check-exn (regexp (regexp-quote "VVQS invalid input function"))
           (lambda () (parse-prog '(1 2 3))))


; ---------------------------------------------------

; Interprets the function named main from the function definitions.
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (define (main-fn) (find-main funs))
  (if (eq? (length (FundefC-args (main-fn))) 0)
      (interp (AppC (IdC 'main) (list)) funs)
      (interp (AppC (IdC 'main) (list (NumC 0))) funs)))

; Finds the function named main in the list of FundefC
(define (find-main [funs : (Listof FundefC)]) : FundefC
  (cond [(empty? funs) (error 'find-main "VVQS main function not found")]
        [(eq? (FundefC-name (first funs)) 'main) (first funs)]
        [else (find-main (rest funs))]))


; Finds the function with the given IdC in the list of FundefC
(define (find-function [f : ExprC] [funs : (Listof FundefC)]) : (U FundefC #f)
  (cond [(empty? funs) #f]
        [(eq? (FundefC-name (first funs)) (IdC-s (cast f IdC))) (first funs)]
        [else (find-function f (rest funs))]))

(check-equal? (find-function (IdC 'a)
                             (parse-prog '{{def {f x} = {+ x 14}}
                                           {def {main init} = {f 2}}})) #f)
(check-exn (regexp (regexp-quote "VVQS function not found:"))
           (lambda () (interp (AppC (IdC 'a) (list(NumC 3)))
                              (parse-prog '{{def {f x} = {+ x 14}}
                                           {def {main init} = {f 2}}}))))



; Substitute all the arguments
(define (subst-all [args : (Listof Sexp)] [vals : (Listof ExprC)] [body : ExprC]) : ExprC
  (cond [(empty? args) body]
        [else (subst-all (rest args) (rest vals) (subst (first args) (first vals) body))]))

; Substitutes the given argument with the given value in the body of the function.
(define (subst [arg : Sexp] [val : ExprC] [body : ExprC]) : ExprC
  (match body
    [(NumC n) body]
    [(IdC s) (if (eq? s arg) val body)]
    [(BinopC op l r) (BinopC op (subst arg val l) (subst arg val r))]
    [(AppC f (list args ...)) (AppC (subst arg val f) (map (λ ([x : ExprC]) (subst arg val x)) args))]
    [(leq0 test then else)
     (leq0 (subst arg val test) (subst arg val then) (subst arg val else))]))

(check-equal? (interp-fns (parse-prog '{{def {f x} = {+ x 14}}
                                        {def {main init} = {f 2}}}))
              16)

(check-exn (regexp (regexp-quote "VVQS argument mismatch:"))
          (lambda () (interp (AppC (IdC 'f) (list (NumC 1)))
                             (list (FundefC 'f (list 'x 'd) (NumC 1)) (FundefC 't (list 'b) (NumC 6))))))


(check-equal? (interp-fns (list (FundefC 'main (list (quote init)) (NumC 2)))) 2)
(check-equal? (interp-fns
               (parse-prog '{{def {f} = 5}
                             {def {main} = {+ {f} {f}}}}))
              10)
(check-exn (regexp (regexp-quote "VVQS main function not found"))
           (lambda () (interp-fns (list (FundefC 'not-main (list (quote init)) (NumC 2))))))

; ---------------------------------------------------

; Takes in a sexp (some code from our language) and evaluates it
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))

(check-equal? (top-interp '{{def {f x y} = {+ x y}}
                            {def {main init} = {f 2 3}}})
              5)
(check-equal? (top-interp '{{def {f x} = {+ x 14}}
                            {def {main init} = {f 2}}})
              16)
(check-equal? (top-interp '{{def {decrement x} = {leq0? x then x else {- x 1}}}
                             {def {main init} = {decrement 1}}})
              0)
(check-equal? (top-interp '{{def {decrement x} = {leq0? x then x else {- x 1}}}
                             {def {main init} = {decrement 0}}})
              0)
(check-equal? (top-interp '((def (realtwice x) = (+ x x)) 
                            (def (main init) = (twice 15)) 
                            (def (twice x) = (realtwice x))))
              30)

(check-exn (regexp (regexp-quote "VVQS Division by zero"))
           (lambda () (top-interp '((def (ignoreit x) = (+ 3 4)) (def (main) = (ignoreit (/ 1 (+ 0 0))))))))

