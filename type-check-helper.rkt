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

(struct function-type (arity))

;; TypeCheckResult = (U 'number function-type? 'bad)
;; number-type-check-fun : Syntax[ExpandedExpression] -> TypeCheckResult
(define (number-type-check-fun ee)
  (number-type-check-fun* (make-immutable-free-id-table) ee))

;; id-table stores values that are either:
;; - 'number
;; - (function-type arity)
;; - 'bad
;; number-type-check-fun* : immutable-free-id-table? Syntax[ExpandedExpression] -> TypeCheckResult
(define (number-type-check-fun* id-table ee)
  (define (loop . ees) (loop-exprs id-table ees))
  (define (loop-exprs t ees) (andmap (lambda (ee) (number-type-check-fun* t ee))
                                     (stx->list ees)))
  (define (update-id-table idents)
    (for/fold ([t id-table])
              ([ident idents])
      (free-id-table-set t ident 'number)))
  (syntax-parse ee
    #:literal-sets (kernel-literals)
    #:literals (+ - * /)
    [(quote num:number) 'number]
    [var:id (free-id-table-ref id-table #'var 'bad)]
    [lam:Lambda
     (loop-exprs
      (update-id-table (car (attribute lam.ast)))
      #'(lam.body ...))]
    [(let-values ([(x:id) rhs-lam:Lambda]) body ...)
     (if (eq? 'number (loop #'rhs-lam))
         (loop-exprs
          (free-id-table-set id-table #'x (function-type (length (car (attribute rhs-lam.ast)))))
          #'(body ...))
         'bad)]
    [(letrec-values ([(x:id) rhs-lam:Lambda]) body ...)
     (let ([arity (length (car (attribute rhs-lam.ast)))])
       (loop-exprs
        (free-id-table-set
         id-table
         #'x
         (let ([body-type (loop-exprs
                           (free-id-table-set id-table #'x (function-type arity))
                           (list #'rhs-lam))])
           (if (eq? 'number body-type)
               (function-type arity)
               'bad)))
        #'(body ...)))]
    [((~or* let-values letrec-values) ([(x:id) rhs]) body ...)
     (loop-exprs (free-id-table-set id-table #'x (loop #'rhs)) #'(body ...))]
    [(#%plain-app (~or* + - * /) arg1 arg2)
     (loop #'arg1 #'arg2)]
    [(#%plain-app lam:Lambda arg ...)
     (let ([param-count (length (car (attribute lam.ast)))])
       (if (eq? 'number (loop-exprs id-table #'(arg ...)))
           (or (and (= param-count (length (syntax->list #'(arg ...))))
                    (loop #'lam))
               'bad)
           'bad))]
    [(#%plain-app proc:id arg ...)
     (let ([id-val (free-id-table-ref id-table #'proc)])
       (or (and (function-type? id-val)
                (= (function-type-arity id-val) (length (syntax->list #'(arg ...))))
                (loop-exprs id-table #'(arg ...)))
           'bad))]
    [_ 'bad]))

(require rackunit)

(check-equal?
 (number-type-check-fun #'(quote 1))
 'number)

(check-equal?
 (number-type-check-fun #'(quote (quote "a")))
 'bad)

(check-equal?
 (number-type-check-fun #'(let-values ([(a) (quote 1)]) a))
 'number)

(check-equal?
 (number-type-check-fun #'(let-values ([(a) (quote 1)]) (#%plain-app + (quote 1) a)))
 'number)

(check-equal?
 (number-type-check-fun #'(let-values (((a) (quote "a"))) a))
 'bad)

(check-equal?
 (number-type-check-fun #'(#%plain-app (#%plain-lambda (x) x) (quote 5)))
 'number)

(check-equal?
 (number-type-check-fun #'(#%plain-app (#%plain-lambda (x) x) (quote "a")))
 'bad)

(check-equal?
 (number-type-check-fun #'(#%plain-app (#%plain-lambda (x) (#%plain-app + (quote 5) x)) (quote 5)))
 'number)

(check-equal?
 (number-type-check-fun #'(let-values ([(foo) (#%plain-lambda (x) (#%plain-app + (quote 1) x))])
                            (#%plain-app foo (quote 10))))
 'number)

(check-equal?
 (number-type-check-fun #'(let-values ([(foo) (#%plain-lambda (x) x)])
                            (#%plain-app foo (quote "a"))))
 'bad)

(check-equal?
 (number-type-check-fun #'(letrec-values ([(foo) (#%plain-lambda (x) x)])
                            (#%plain-app foo (quote "a"))))
 'bad)

(check-equal?
 (number-type-check-fun #'(letrec-values ([(foo) (#%plain-lambda (x) foo)])
                            (#%plain-app foo (quote "a"))))
 'bad)

(check-equal?
 (number-type-check-fun #'(letrec-values ([(foo) (#%plain-lambda (x) foo)])
                            (#%plain-app foo (quote 1))))
 'bad)

(check-equal?
 (number-type-check-fun #'(letrec-values ([(foo) (#%plain-lambda (x) (#%plain-app foo x))])
                            (#%plain-app foo (quote 1))))
 'number)
