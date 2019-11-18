#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/stx racket/match))

(module+ test
  (require rackunit syntax/macro-testing))

;; ------------------------------------------------------------

;; A "local variable" is a variable that is not imported from another module.
;; That is, it is bound by `let`, `lambda`, etc or it is defined in the current
;; module or at the REPL. (In practice, macros can't always distinguish between
;; "defined at the REPL" and unbound.)

(begin-for-syntax
  ;; local-var? : Identifier -> Boolean
  ;; Returns #t if id is not imported from another module; that is, it is bound
  ;; locally or unbound.
  (define (local-var? id)
    (match (identifier-binding id)
      [(list* src-mod src-sym _)
       ;; defined in module; return #t if src-mod is the current module, #f otherwise
       (let-values ([(mod base) (module-path-index-split src-mod)]) (eq? mod #f))]
      [_ #t])))

(module+ test
  (let ([x 10])
    (check-equal? (phase1-eval (local-var? #'x)) #t))
  (define zzz 20)
  (check-equal? (phase1-eval (local-var? #'zzz)) #t)
  (check-equal? (phase1-eval (local-var? #'list)) #f))


;; ------------------------------------------------------------

;; Write a macro `must-be-closed` that takes an expression and raises
;; a syntax error if it contains any *free local variables*. If the expression
;; `e` has no free local variables, then `(must-be-closed e)` should
;; behave the same as `e`.

;; Examples:
;; (must-be-closed (lambda (x) (+ x 5))) ;; ok
;; (lambda (x) (must-be-closed x)) ;; error

;; Hint: use local-expand.


(begin-for-syntax

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
    (filter not-member-of-ys xs)))



(define-syntax (must-be-closed stx)
  (syntax-parse stx
    [(_ e:expr) (let ([lfv (local-free-vars (local-expand #'e 'expression null))])
                  (if (null? lfv)
                      #'e
                      (raise-syntax-error #f "expression has free local variables" stx (car lfv))))]))



(module+ test
  ;; For comparison:
  (define fact-of-10 (for/product ([i (in-range 10)]) (add1 i)))

  ;; Okay
  (check-equal?
   (convert-syntax-error
    (let ([x 20])
      (must-be-closed
       (let ()
         (define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
         (fact 10)))))
   fact-of-10)

  (check-exn
   #rx"expression has free local variables"
   (lambda ()
     (convert-syntax-error
      (let ([foo-rec 10])
        (must-be-closed
         (let ([foo-rec
                (lambda (x y)
                  (if (= x 0) y (foo-rec (sub1 x) (* 2 y))))])
           (foo-rec 10 2)))))))
  
  ;; For comparison:
  (define let-test (letrec ([foo-rec
                             (lambda (x y)
                               (if (= x 0) y (foo-rec (sub1 x) (* 2 y))))])
                     (foo-rec 10 2)))

  ;; Okay
  (check-equal?
   (convert-syntax-error
    (let ([foo-rec 10])
      (must-be-closed
       (letrec ([foo-rec
                 (lambda (x y)
                   (if (= x 0) y (foo-rec (sub1 x) (* 2 y))))])
         (foo-rec 10 2)))))
   let-test)
  
;(let ([x 20]) (must-be-closed (letrec () x)))
  

  (check-exn
   #rx"expression has free local variables"
   (lambda ()
     (convert-syntax-error
      (let ([x 20])
        (must-be-closed
         (let ()
           (define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
           (fact (quotient x 2))))))))

  (check-exn
   #rx"expression has free local variables"
   (lambda ()
     (convert-syntax-error
      (let ([x 20])
        (must-be-closed
         (let ()
           (define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
           (fact (quotient zzz 2)))))))))



;; ============================================================

(require racket/pretty racket/match)

;; A PrettyClosure is (pretty-closure Any (Listof Symbol) (Listof Any) Procedure).
;; Note: fvs and fv-vals must have the same length, and that must also be the
;; arity of proc.
(struct pretty-closure (code fvs fv-vals proc)
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (lambda (self out mode)
    (match-define (pretty-closure code fvs fv-vals proc) self)
    (pretty-write `(let ,(map list fvs fv-vals) ,code) out #:newline? #f)))

;; call-closure : PrettyClosure -> Any
(define (call-closure pc)
  (apply (pretty-closure-proc pc) (pretty-closure-fv-vals pc)))

;; ------------------------------------------------------------

;; Write a macro `reify-closure` that takes an expression and returns
;; a PrettyClosure containing the following information:
;; - an S-expression representing the original expression
;; - a list of symbols, the names of the free local variables (Each
;;   free variable should occur in the list only once!)
;; - a list of the free variables' values
;; - a procedure that takes as many arguments as the expression has
;;   free local variables and runs the original expression with
;;   them. (You may assume that none of the variables are mutated.)

;; Example:
;; (let ([y 10]) (reify-closure (lambda (x) y)))
;; =>
;; (pretty-closure '(lambda (x) y) '(y) '(10) (lambda (y) (lambda (x) y)))

(begin-for-syntax
  ;; cons-distinct : Identifier (Listof Identifier) -> (Listof Identifier)
  (define (cons-distinct el lst)
    (define (not-member-of-lst x) (not (for/or ([y (in-list lst)]) (free-identifier=? x y))))
    (if (not-member-of-lst el) (cons el lst) lst)))

(require (for-syntax racket/list))

(define-syntax (reify-closure stx)
  (syntax-parse stx
    [(_ e:expr) (let* ([ee (local-expand #'e 'expression null)]
                       [lfv (remove-duplicates (local-free-vars ee) free-identifier=?)])
                  #`(pretty-closure 'e '#,lfv (list #,@lfv) (lambda #,lfv #,ee)))]))

(let ([y 10]) (reify-closure (lambda (x) (cons y y))))