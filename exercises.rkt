#lang racket

; Excs 1
(define-syntax-rule (noisy-v1 expr)
  (begin (printf "evaluating ~a~n" expr)
         expr))

(display "Excs 1\n")
(noisy-v1 5)

; Excs 2
(define-syntax-rule (noisy-v2 expr)
  (begin (printf "evaluating ~a~n" expr)
         (begin0 expr
                 (printf "done~n"))))

(display "\nExcs 2\n")
(noisy-v2 5)

; Excs 3
(define-syntax (iflet stx)
  (syntax-case stx ()
    [(_ var-id e-cond e1 e2) (identifier? #'var-id)
                             #'(let ([e e-cond])
                                 (if e (let ([var-id e]) e1) e2))]))

(require (for-syntax racket/base syntax/parse))

(define-syntax (iflet1 stx)
  (syntax-parse stx
    [(_ var-id:id e-cond e1 e2)
     #'(let ([e e-cond]) (if e (let ([var-id e]) e1) e2))]))

(display "\nExcs 3\n")
(define alist '((1 . apple) (2 . pear)))
(equal? (iflet x (assoc 1 alist) (cdr x) 'none) 'apple)
(equal? (let ([x 'plum]) (iflet x (assoc 3 alist) (cdr x) x)) 'plum)
(equal? (let ([x 'apple]) (iflet x
                                (begin (display "noisy\n") (assoc 1 alist))
                                (cdr x) 'none)) 'apple)

(equal? (iflet1 x (assoc 1 alist) (cdr x) 'none) 'apple)
(equal? (let ([x 'plum]) (iflet1 x (assoc 3 alist) (cdr x) x)) 'plum)

; Excs 4
(define-syntax-rule (forever expr)
  (begin expr (forever expr)))

;(forever (display "noisy\n"))

; Excs 5
(define-syntax-rule (handle expr1 expr2)
  (with-handlers ([exn:fail? (lambda (e) expr2)])
    expr1))

(display "\nExcs 5\n")
(equal? (handle 5 6) 5)
(equal? (handle (/ 1 0) 'whoops) 'whoops)

; Excs 6
(define-syntax-rule (forever1 e)
  (forever-fun (lambda () e)))

(define (forever-fun thunk)
  (begin (thunk) (forever-fun thunk)))

(define-syntax-rule (handle1 e1 e2)
  (handle-fun (lambda () e1) (lambda () e2)))

(define (handle-fun thunk1 thunk2)
  (with-handlers ([exn:fail? (lambda (e) (thunk2))])
    (thunk1)))

(display "\nExcs 6\n")
(equal? (handle1 5 6) 5)
(equal? (handle1 (/ 1 0) 'whoops) 'whoops)

; Excs 7
(define-syntax-rule (andlet1 var e1 e2)
  (andlet1-fun 'var 'e1 'e2))

(define (andlet1-fun var e1 e2)
  (let ([ns (make-base-namespace)])
    (eval `(let ([,var ,e1])
             (if ,var ,e2 #f)) ns)))

(display "\nExcs 7\n")
(andlet1 x 2 (+ x 1))

; Excs 8
(define-syntax-rule (test name actual expected)
  (test-fun name (lambda () actual) (lambda () expected)))

(define (test-fun name th-actual th-expected)
  (let ([failed (lambda () (printf "test ~a failed\n" name))])
    (with-handlers ([exn:fail? (lambda (e) (failed))])
      (if (equal? (th-actual) (th-expected))
          (void)
          (failed)))))

(display "\nExcs 8\n")
(test "test-macro" 5 5)
(test "test-macro" 5 3)
(test "test-macro" 5 (/ 1 0))
(test "test-macro" (/ 1 0) 5)

; Excs 10, 14
(define-syntax my-and
  (syntax-rules ()
    [(my-and a) a]
    [(my-and a b c ...) (if a (my-and b c ...) #f)]))

(define-syntax my-or
  (syntax-rules ()
    [(my-or a) a]
    [(my-or a b c ...) (if a #t (my-or b c ...))]))

(display "\nExcs 10, 14\n")
(my-and #t #t #t)
(my-and #t #f #t)
(my-or #f #f #f)
(my-or #f #f #t)

; Excs 11
(define-syntax-rule (my-let ([var-id rhs-expr] ...) body-expr)
  ((lambda (var-id ...) body-expr) rhs-expr ...))

(display "\nExcs 11\n")
;(my-let ([5 2]) 3)
;(my-let ([x 2] [x 5]) 3)
(my-let ([:#level 2]) 5)
(my-let ([[x 3] 2]) 5)

; Excs 13
(define-syntax-rule (my-cond-v0 [question-expr answer-expr] ...)
  (or (and question-expr answer-expr) ...))

(display "\nExcs 13\n")
(my-cond-v0
   [(positive? -5) (error "doesn't get here")]
   [(zero? -5) (error "doesn't get here, either")]
   [(positive? 5) 'here]
   [(member 2 '(1 2 3)) 'there])

; Excs 15, 16
(define-syntax my-cond
  (syntax-rules (else)
    [(my-cond)
     (void)]
    [(my-cond [else answer-expr])
     answer-expr]
    [(my-cond [question-expr => answer-expr] clause ...)
     (let ([qe question-expr])
       (if qe
           (answer-expr qe)
           (my-cond clause ...)))]
    [(my-cond [question-expr] clause ...)
     (or question-expr
         (my-cond clause ...))]
    [(my-cond [question-expr answer-expr ...] clause ...)
     (if question-expr
         (begin answer-expr ...)
         (my-cond clause ...))]))

(display "\nExcs 15, 16\n")
(my-cond
   [(member 2 '(1 2 3)) => (lambda (l) (map - l))]
   [(positive? 5) 'here])

(my-cond
   [(member 4 '(1 2 3)) => (lambda (l) (map - l))]
   [(positive? 5)])

(my-cond
   [(positive? -5) (error "doesn't get here")]
   [(zero? -5) (error "doesn't get here, either")]
   [(member 4 '(1 2 3)) => (lambda (l) (map - l))]
   [(positive? 5) (display "hi!\n") 'here 'and 'there]
   [(member 2 '(1 2 3)) => (lambda (l) (map - l))])

; Excs 17
(define-syntax-rule (minimatch1 val-expr mm-pattern result-expr)
  (let ([v val-expr])
    (minimatch1* v mm-pattern result-expr)))

(define-syntax (minimatch1* stx)
  (syntax-parse stx
    #:literals (cons quote)
    [(_ val var-id:id res-expr)
     #'(let ([var-id val]) res-expr)]
    [(_ val (quote datum) res-expr)
     #'(if (equal? val (quote datum))
         res-expr
         (error "match error"))]
    [(_ val (cons first-mm-pattern rest-mm-pattern) res-expr)
     #'(if (cons? val)
           (minimatch1* (car val) first-mm-pattern
                        (minimatch1* (cdr val) rest-mm-pattern res-expr))
           (error "match error"))]))

(minimatch1 (cons 5 '2) (cons x v) (+ x v))
(minimatch1 (list 1 2 3) (cons x y) y)
(minimatch1 (list 1 2 3) (cons cons (cons x y)) y)