#lang racket

(require racket/base syntax/parse syntax/stx syntax/id-table
         (for-template racket/base))

(provide (all-defined-out))

(define (local-var? id)
    (match (identifier-binding id)
      [(list* src-mod src-sym _)
       ;; defined in module; return #t if src-mod is the current module, #f otherwise
       (let-values ([(mod base) (module-path-index-split src-mod)]) (eq? mod #f))]
      [_ #t]))

(define-syntax-class formals
    (pattern (id:id ...) #:attr ast (syntax->list #'(id ...)))
    (pattern (id:id ...+ . rest-id:id) #:attr ast (cons #'rest-id (syntax->list #'(id ...))))
    (pattern rest-id #:attr ast (list #'rest-id)))

;; local-free-vars : Syntax[ExpandedExpression] -> (Listof Identifier)
  ;; Returns a list of occurrences of local free vars of the expression.
  ;; Duplicates are not removed.
  (define (local-free-vars ee)
    (define (loop . ees) (loop-exprs ees))
    (define (loop-exprs ees) (apply append (map local-free-vars (stx->list ees))))
    (syntax-parse ee
      #:literal-sets (kernel-literals)
      [var:id
       (if (local-var? #'var) (list #'var) null)]
      [(#%plain-lambda f:formals body ...)
       (subtract-ids (loop-exprs #'(body ...)) (attribute f.ast))]
      [(case-lambda [f:formals body ...] ...)
       (loop-exprs #'((#%plain-lambda f body ...) ...))]
      [(if e1 e2 e3)
       (loop #'e1 #'e2 #'e3)]
      [(begin e ...)
       (loop-exprs #'(e ...))]
      [(begin0 e0 e ...)
       (loop-exprs #'(e0 e ...))]
      [(let-values ([(x:id ...) rhs] ...) body ...)
       (loop-exprs #'((#%plain-lambda (x ... ...) body ...) rhs ...))]
      ; (loop #'(#%plain-lambda (x ... ...) rhs ... body ...))]
      [(letrec-values ([(x:id ...) rhs] ...) body ...)
       (loop #'(#%plain-lambda (x ... ...) rhs ... body ...))]
      [(letrec-syntaxes+values _ ([(x:id ...) rhs] ...) body ...)
       (loop #'(#%plain-lambda (x ... ...) rhs ... body ...))]
      [(set! x:id rhs)
       (loop #'x #'rhs)]
      [(with-continuation-mark e1 e2 e3)
       (loop #'e1 #'e2 #'e3)]
      [(#%plain-app e ...)
       (loop-exprs #'(e ...))]
      ;; None of the other forms (eg, quote) can contain variable references.
      [_ null]))

  ;; subtract-ids : (Listof Identifier) (Listof Identifier) -> (Listof Identifier)
  (define (subtract-ids xs ys)
    (define (not-member-of-ys x) (not (for/or ([y (in-list ys)]) (free-identifier=? x y))))
    (filter not-member-of-ys xs))

(define-syntax-class Lambda
  #:literal-sets (kernel-literals)
  (pattern (#%plain-lambda f:formals body ...)
           #:attr ast (cons (attribute f.ast) (syntax->list #'(body ...)))))

;; number-type-check-fun : Syntax[ExpandedExpression] -> Boolean
(define (number-type-check-fun ee)
  (and
   (null? (local-free-vars ee))
   (number-type-check-fun* (make-immutable-free-id-table) ee)))

;; number-type-check-fun* : immutable-free-id-table? Syntax[ExpandedExpression] -> Boolean
(define (number-type-check-fun* id-table ee)
  (define (loop . ees) (loop-exprs id-table ees))
  (define (loop-exprs t ees) (andmap (lambda (ee) number-type-check-fun* t ee)
                                     (stx->list ees)))
  (syntax-parse ee
      #:literal-sets (kernel-literals)
      #:literals (+ - * /)
      [num:number #t]
      [var:id (free-id-table-ref #'var #f)]
      [lam:Lambda
       (loop-exprs id-table (cdr (attribute lam.ast)))]
      [((~or* let-values letrec-values) ([(x:id) rhs-lam:Lambda]) body ...)
       (loop-exprs
        (free-id-table-set id-table #'x (cons (length (car (attribute rhs-lam.ast)))
                                              (loop #'rhs-lam)))
        #'(body ...))]
      [(let-values ([(x:id) rhs]) body ...)
       (loop-exprs (free-id-table-set id-table #'x (loop #'rhs)) #'(body ...))]
      [(letrec-values ([(x:id) rhs]) body ...)
       (loop-exprs
        (free-id-table-set id-table #'x (loop-exprs (free-id-table-set id-table #'x #t)
                                                    (list #'rhs)))
        #'(body ...))]
      [(#%plain-app (~or* + - * /) arg1 arg2)
       (loop #'arg1 #'arg2)]
      [(#%plain-app lam:Lambda arg ...)
       (let ([param-count (length (car (attribute lam.ast)))])
         (and (= param-count (length (syntax->list #'(arg ...))))
              (loop-exprs id-table (cdr (attribute lam.ast)))))]
      [(#%plain-app proc arg ...)
       (let ([id-val (loop #'proc)])
         (and (pair? id-val)
              (and (= (car id-val) (length (syntax->list #'(arg ...))))
                   (cdr id-val))))]
      [_ #f]))

(require rackunit)

(check-equal?
 (number-type-check-fun (let-values (((a) "a")) a))
 #f)

(check-equal?
 (number-type-check-fun (#%plain-app (lambda (x) x) 5))
 #t)

(check-equal?
 (number-type-check-fun (#%plain-app (lambda (x) (#%plain-app + 1 x)) 5))
 #t)

(check-equal?
 (number-type-check-fun (let-values ([(foo) (lambda (x) (#%plain-app + 1 x))])
                          (#%plain-app foo 10)))
 #t)

(check-equal?
 (number-type-check-fun (let-values ([(foo) (lambda (x) x)])
                          (#%plain-app foo "a")))
 #f)

(define something 5)
(check-equal?
 (number-type-check-fun (let-values ([(foo) (lambda (x) x)])
                          (#%plain-app foo something)))
 #f)