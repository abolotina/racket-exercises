#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket)

;; ------------------------------------------------------------
;; Ex 1: Extend minimatch with two new pattern forms, ? and app.

;; A pattern of the form (? predicate sub-pattern ...) matches a value
;; if the predicate returns a true value when applied to the value and
;; if every sub-pattern matches the value. The variables bound by the
;; ?-pattern are all of the the variables of the sub-patterns.

;; A pattern of the form (app transform sub-pattern) matches a value
;; V1 if sub-pattern matches V2, where V2 is the value obtained by
;; applying transform to V1. The variables bound by the app-pattern
;; are the same as the varaiables of the sub-pattern.


;; (match 6
;;   [(? (lambda (x) (or (symbol? x) (string? x)))) 'symbol-or-string]
;;   [(? (lambda (x) (and (number? x) (> x 5)))) 'number/>5]) = 'number/>5
;; (match '(1 2 3)
;;   [(? pair? (app car (? number? x))) x]) = 1

(define-syntax-rule (minimatch val-expr clause ...)
  (let ([v val-expr])
    (minimatch* v clause ...)))

(define-syntax minimatch*
  (syntax-rules ()
    [(minimatch* v)
     (error 'minimatch "match failed")]
    [(minimatch* v [pattern res-expr] clause ...)
       (minimatch** v pattern res-expr (minimatch* v clause ...))]))

(define-syntax-rule (?) ())
(define-syntax-rule (app) ())

(require (for-syntax racket/struct-info racket/syntax))

(define-syntax (minimatch** stx)
  (syntax-parse stx
    #:literals (cons quote ? app)
    [(_ val var-id:id res-expr match-rest)
     #'(let ([var-id val]) res-expr)]
    [(_ val (quote datum) res-expr match-rest)
     #'(if (equal? val (quote datum))
         res-expr
         match-rest)]
    [(_ val (cons first-pattern rest-pattern) res-expr match-rest)
     #'(if (cons? val)
           (minimatch** (car val) first-pattern
                        (minimatch** (cdr val) rest-pattern res-expr match-rest)
                        match-rest)
           match-rest)]
    [(_ val (? expr) res-expr match-rest)
     #'(if (expr val)
           res-expr
           match-rest)]
    [(_ val (? expr pat1 pat2 ...) res-expr match-rest)
     #'(if (expr val)
           (minimatch** val pat1
                        (minimatch** val (? (lambda (x) #t) pat2 ...)
                                     res-expr
                                     match-rest)
                        match-rest)
           match-rest)]
    [(_ val (app expr pat) res-expr match-rest)
     #'(minimatch** (expr val) pat res-expr match-rest)]
    [(_ val (app expr pat1 pat2 ...) res-expr match-rest)
     #'(let ([results (call-with-values (lambda () (expr val)) list)])
         (minimatch** (car results) pat1
                      (minimatch** (cdr results)
                                   (app (lambda (ls) (apply values ls)) pat2 ...)
                                   res-expr
                                   match-rest)
                      match-rest))]
    [(_ val (struct-id:id field-val ...) res-expr match-rest)
     (let* ([struct-info (extract-struct-info (syntax-local-value #'struct-id))]
            [struct-pred (caddr struct-info)]
            [fields (reverse (cadddr struct-info))]
            [fields-count (length fields)]
            [match-fields
             (with-syntax ([(field-acc ...) fields])
               (if (< 0 fields-count)
                   (list #'(app (lambda (v) (values (field-acc v) ...))
                                field-val ...))
                   null))])
       (if (= fields-count (length (syntax->list #'(field-val ...)))) 
           #`(minimatch** val
                          (? #,struct-pred #,@match-fields)
                          res-expr
                          match-rest)
           (raise-syntax-error #f (format "invalid number of fields for structure ~s"
                                          (syntax->datum (cadr struct-info)))
                               stx)))]))

(require rackunit)

(check-equal?
 (minimatch (list 1 2 3)
            ['3 'here]
            [(? list? x (cons a b)) x]
            [(cons x y) y]
            [x x])
 '(1 2 3))

(check-equal?
 (minimatch (list 1 2 3)
            ['3 'here]
            [(? number? x (cons a b)) x]
            [(cons x y) y]
            [x x])
 '(2 3))

(check-equal?
 (minimatch 6
            [(? (lambda (x) (or (symbol? x) (string? x)))) 'symbol-or-string]
            [(? (lambda (x) (and (number? x) (> x 5)))) 'number/>5])
 'number/>5)

(check-equal?
 (minimatch '(1 2 3)
            [(? pair? (app car (? number? x))) x])
 1)

(check-equal?
 (minimatch '(1 2)
            [(app length '2) 'yes])
 'yes)

(check-equal?
 (minimatch "3.14"
            [(app string->number (? number? pi))
             `(I got ,pi)])
 '(I got 3.14))

(check-equal?
 (minimatch '(1 2)
            [(app (lambda (v) (split-at v 1)) '(1) '(2)) 'yes])
 'yes)

(check-equal?
 (minimatch '(1 2 3)
            [(app (lambda (ls) (apply values ls)) x y (? odd? z))
             (list 'yes x y z)])
 '(yes 1 2 3))

;; ------------------------------------------------------------
;; Ex 2: Extend minimatch to support struct patterns.

;; A struct pattern is a pattern of the form (struct-name sub-pattern
;; ...), where struct-name is the name of a struct type, and there are
;; as many sub-patterns as the struct has fields. It matches a value
;; if the value is an instance of that struct and if each sub-pattern
;; matches the corresponding field of the value. The variables bound
;; by the struct pattern are all of the variables of the sub-patterns.

;; Hint: use extract-struct-info, and see the whole page where that
;; procedure is documented

(struct point (x y))
(define origin (point 0 0))

(check-equal?
 (minimatch origin
            [(point (? zero? a) b) (list a b)])
 '(0 0))

(check-equal?
 (minimatch origin
            [(point (? zero? a) '0) a])
 0)

(struct tree (val left right))

(check-equal?
 (minimatch (tree 0 (tree 1 #f #f) #f)
            [(tree a (tree b  '#f '#f) '#f) (list a b)])
 '(0 1))

(struct nil ())

(check-equal?
 (let ([z (nil)])
   (minimatch z
              [(nil) 1]))
 1)

;; ------------------------------------------------------------
;; Ex 3: Extend minimatch to support pattern-transformers, and provide
;; a define-pattern-transformer form to define new pattern
;; transformers.

;; Pattern transformers allow the *users* of minimatch to extend its
;; language of patterns. For example, one could add support for list
;; patterns with the following definition:

;;   (define-pattern-transformer List
;;     (lambda (stx)
;;       (syntax-parse stx
;;         [(_)
;;          #'(quote ())]
;;         [(_ p1 p ...)
;;          #'(cons p1 (List p ...))])))

;; With that definition, minimatch should support List patterns: A
;; pattern of the form (List sub-pattern ...) should match a value if
;; the value is a list whose length is the same as the number of
;; sub-patterns, and if each sub-pattern matches the corresponding
;; element of the list. The List-pattern binds the variables of all of
;; its sub-patterns.
