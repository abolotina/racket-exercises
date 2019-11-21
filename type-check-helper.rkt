#lang racket

(require racket/base syntax/parse syntax/stx syntax/id-table
         (for-template racket/base))

(provide (all-defined-out))

(define-syntax-class formals
  (pattern (id:id ...) #:attr ast (syntax->list #'(id ...)))
  (pattern (id:id ...+ . rest-id:id) #:attr ast (cons #'rest-id (syntax->list #'(id ...))))
  (pattern rest-id #:attr ast (list #'rest-id)))

(define-syntax-class Lambda
  #:literal-sets (kernel-literals)
  (pattern (#%plain-lambda f:formals body ...)
           #:attr ast (cons (attribute f.ast) (syntax->list #'(body ...)))))

;; number-type-check-fun : Syntax[ExpandedExpression] -> Boolean
(define (number-type-check-fun ee)
  (number-type-check-fun* (make-immutable-free-id-table) ee))

;; number-type-check-fun* : immutable-free-id-table? Syntax[ExpandedExpression] -> Boolean
(define (number-type-check-fun* id-table ee)
  (define (loop . ees) (loop-exprs id-table ees))
  (define (loop-exprs t ees) (andmap (lambda (ee) (number-type-check-fun* t ee))
                                     (stx->list ees)))
  (syntax-parse ee
    #:literal-sets (kernel-literals)
    #:literals (+ - * /)
    [(quote num:number) #t]
    [var:id (let ([val (free-id-table-ref id-table #'var #f)])
              (and val (boolean? val)))]
    [lam:Lambda
     (loop-exprs
      (letrec ([update-id-table
                (lambda (t ids)
                  (match ids
                    ['() t]
                    [(cons ident tail) (update-id-table
                                        (free-id-table-set t ident #t)
                                        tail)]))])
        (update-id-table id-table (car (attribute lam.ast))))
      (cdr (attribute lam.ast)))]
    [(let-values ([(x:id) rhs-lam:Lambda]) body ...)
     (loop-exprs
      (free-id-table-set id-table #'x (cons (length (car (attribute rhs-lam.ast)))
                                            (loop #'rhs-lam)))
      #'(body ...))]
    [(let-values ([(x:id) rhs]) body ...)
     (loop-exprs (free-id-table-set id-table #'x (loop #'rhs)) #'(body ...))]
    [(letrec-values ([(x:id) rhs-lam:Lambda]) body ...)
     (let ([arity (length (car (attribute rhs-lam.ast)))])
       (loop-exprs
        (free-id-table-set
         id-table
         #'x
         (cons arity (loop-exprs
                      (free-id-table-set id-table #'x (cons arity #t))
                      (list #'rhs-lam))))
        #'(body ...)))]
    [(#%plain-app (~or* + - * /) arg1 arg2)
     (loop #'arg1 #'arg2)]
    [(#%plain-app lam:Lambda arg ...)
     (let ([param-count (length (car (attribute lam.ast)))])
       (and (= param-count (length (syntax->list #'(arg ...))))
            (loop #'lam)
            (loop-exprs id-table #'(arg ...))))]
    [(#%plain-app proc:id arg ...)
     (let ([id-val (free-id-table-ref id-table #'proc)])
       (and (pair? id-val)
            (= (car id-val) (length (syntax->list #'(arg ...))))
            (cdr id-val)
            (loop-exprs id-table #'(arg ...))))]
    [_ #f]))

(require rackunit)


(check-equal?
 (number-type-check-fun #'(quote 1))
 #t)

(check-equal?
 (number-type-check-fun #'(quote (quote "a")))
 #f)

(check-equal?
 (number-type-check-fun #'(let-values ([(a) (quote 1)]) a))
 #t)

(check-equal?
 (number-type-check-fun #'(let-values ([(a) (quote 1)]) (#%plain-app + (quote 1) a)))
 #t)

(check-equal?
 (number-type-check-fun #'(let-values (((a) (quote "a"))) a))
 #f)

(check-equal?
 (number-type-check-fun #'(#%plain-app (#%plain-lambda (x) x) (quote 5)))
 #t)

(check-equal?
 (number-type-check-fun #'(#%plain-app (#%plain-lambda (x) x) (quote "a")))
 #f)

(check-equal?
 (number-type-check-fun #'(#%plain-app (#%plain-lambda (x) (#%plain-app + (quote 5) x)) (quote 5)))
 #t)

(check-equal?
 (number-type-check-fun #'(let-values ([(foo) (#%plain-lambda (x) (#%plain-app + (quote 1) x))])
                            (#%plain-app foo (quote 10))))
 #t)

(check-equal?
 (number-type-check-fun #'(let-values ([(foo) (#%plain-lambda (x) x)])
                            (#%plain-app foo (quote "a"))))
 #f)

(check-equal?
 (number-type-check-fun #'(letrec-values ([(foo) (#%plain-lambda (x) x)])
                            (#%plain-app foo (quote "a"))))
 #f)

(check-equal?
 (number-type-check-fun #'(letrec-values ([(foo) (#%plain-lambda (x) foo)])
                            (#%plain-app foo (quote "a"))))
 #f)

(check-equal?
 (number-type-check-fun #'(letrec-values ([(foo) (#%plain-lambda (x) foo)])
                            (#%plain-app foo (quote 1))))
 #f)

(check-equal?
 (number-type-check-fun #'(letrec-values ([(foo) (#%plain-lambda (x) (#%plain-app foo x))])
                            (#%plain-app foo (quote 1))))
 #t)
