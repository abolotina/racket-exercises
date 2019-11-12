#lang racket

(module+ test
  (require rackunit))

;; Write a macro `integer-ranges-predicate` that takes a sequence of closed
;; range declarations and produces a predicate that tests whether a given number
;; is in any of the ranges. See the examples for the syntax.

;; ------------------------------------------------------------
;; v1: Get a simple version working.

(define-syntax-rule (integer-ranges-predicate [start end] ...)
  (lambda (x) (for/or ([s '(start ...)]
                       [e '(end ...)])
                (and (<= s x) (<= x e)))))


;; Examples:

(define small-composite?
  (integer-ranges-predicate
   [4 4] [6 6] [8 10] [12 12] [14 16] [18 18] [20 22] [24 28]))


(module+ test
  (require (only-in math/number-theory prime?))
  (for ([k (in-range 2 30)])
    (test-case (format "number ~s" k)
      (check-equal? (small-composite? k) (not (prime? k))))))

(define ascii-alphanum?
  (integer-ranges-predicate
   [48 57] [65 90] [97 122]))

(module+ test
  (for ([k (in-range 128)])
    (test-case (format "character ~s" k)
      (check-equal? (ascii-alphanum? k)
                    (or (char-alphabetic? (integer->char k))
                        (char-numeric? (integer->char k)))))))


;; ------------------------------------------------------------

;; v2: Generate code that creates a run-time data structure and uses a run-time
;; function to implement its behavior. Your function should perform O(log k)
;; comparisons, where k is the number of ranges. (Specifically, you should use
;; some sort of binary search.)
;;
;; You may assume that the ranges are non-overlapping and in ascending
;; order. (But think about how you would change your macro if you could not make
;; that assumption.)

;; No new examples.

; [4 4] [6 6] [8 10] [12 12]
; =>
;'((8 . 10) ((6 . 6) (4 . 4) ()) (12 . 12))

(define-syntax-rule (integer-ranges-predicate-v2 [start end] ...)
  (lambda (x)
    (integer-ranges-predicate-v2* x '((start . end) ...))))

(define (integer-ranges-predicate-v2* x ranges)
  (letrec ([cons-tree
            (lambda (rs)
              (let ([len (length rs)])
                (cond
                  [(= len 0) rs]
                  [(< len 2) (car rs)]
                  [else
                    (let-values ([(rs-l rs-r) (split-at rs (round (/ len 2)))])
                      (list (car rs-r)
                            (cons-tree rs-l)
                            (cons-tree (cdr rs-r))))])))])
        (int-ranges-pred-fun x (cons-tree ranges))))

(define (int-ranges-pred-fun x ranges)
  (match ranges
    [(list (cons s e) lhs rhs)
     (cond
       [(< x s) (int-ranges-pred-fun x lhs)]
       [(<= x e) #t]
       [else (int-ranges-pred-fun x rhs)])]
    [(cons s e) (<= s x e)]
    [else #f]))

(define small-composite1?
  (integer-ranges-predicate-v2
   [4 4] [6 6] [8 10] [12 12]))

(module+ test
  (check-equal? (small-composite1? 12) (small-composite? 12))
  (check-equal? (small-composite1? 6) (small-composite? 6))
  (check-equal? (small-composite1? 5) (small-composite? 5)))

(define small-composite2?
  (integer-ranges-predicate-v3
   [4 4] [6 6] [8 10] [12 12] [14 16] [18 18] [20 22] [24 28]))


(module+ test
  (require (only-in math/number-theory prime?))
  (for ([k (in-range 2 30)])
    (test-case (format "number ~s" k)
      (check-equal? (small-composite2? k) (not (prime? k))))))

;(define bad-range?
;  (integer-ranges-predicate-v3
;   [4 4] [6 6] [8 10] [12 "adadd"] [14 16] [18 18] [20 22] [24 28]))

(define ascii-alphanum1?
  (integer-ranges-predicate-v3
   [48 57] [65 90] [97 122]))

(module+ test
  (for ([k (in-range 128)])
    (test-case (format "character ~s" k)
      (check-equal? (ascii-alphanum1? k)
                    (or (char-alphabetic? (integer->char k))
                        (char-numeric? (integer->char k)))))))

;; ------------------------------------------------------------
;; v3: Generate code that minimizes (or nearly minimizes) the number of comparisons
;; performed. Specifically, the generated code should do binary search via a tree
;; of `if` expressions (not by traversing a data structure).
;;
;; You may assume that the ranges are non-overlapping and in ascending order.

;; No new examples.

(require (for-syntax syntax/parse)
         (for-syntax racket))

(begin-for-syntax
  ; a closed interval of natural numbers
  (struct range (low high))
  ; code-gen-v3 : (list of range) id -> syntax
  (define (code-gen-v3 ranges x)
    (printf "ranges equals ~e\n" ranges)
    (with-syntax ([x x])
       (let*-values ([(len) (length ranges)])
         (cond
           [(= len 0) #'#f]
           [(= len 1) (with-syntax ([s (range-low (car ranges))]
                                    [e (range-high (car ranges))])
                        #'(<= s x e))]
           [else (let-values ([(rs-l rs-r) (split-at ranges (round (/ len 2)))])
                   (with-syntax ([s (range-low (car rs-r))])
                     #`(if (< x s)
                           #,(code-gen-v3 rs-l #'x)
                           #,(code-gen-v3 rs-r #'x))))])))))

(define-syntax (integer-ranges-predicate-v3 stx)

  (define-syntax-class Range
    (pattern [start:nat end:nat]
             #:attr ast (range (syntax->datum #'start) (syntax->datum #'end)))
    (pattern num:nat #:attr ast (range (syntax->datum #'num) (syntax->datum #'num))))
  (syntax-parse stx
    [(_ r:Range ...)
     #`(lambda (x) #,(code-gen-v3 (attribute r.ast) #'x))]))



(define small-composite1-v3?
  (integer-ranges-predicate-v3
   [4 4] [6 6] [8 10] [12 12]))

(module+ test
  (check-equal? (small-composite1-v3? 12) (small-composite? 12))
  (check-equal? (small-composite1-v3? 6) (small-composite? 6))
  (check-equal? (small-composite1-v3? 5) (small-composite? 5)))

(define small-composite2-v3?
  (integer-ranges-predicate-v3
   [4 4] [6 6] [8 10] [12 12] [14 16] [18 18] [20 22] [24 28]))


(module+ test
  (require (only-in math/number-theory prime?))
  (for ([k (in-range 2 30)])
    (test-case (format "number ~s" k)
      (check-equal? (small-composite2-v3? k) (not (prime? k))))))

(define ascii-alphanum1-v3?
  (integer-ranges-predicate-v3
   [48 57] [65 90] [97 122]))

(module+ test
  (for ([k (in-range 128)])
    (test-case (format "character ~s" k)
      (check-equal? (ascii-alphanum1-v3? k)
                    (or (char-alphabetic? (integer->char k))
                        (char-numeric? (integer->char k)))))))

;; ------------------------------------------------------------
;; v4: Expand the macro to allow overlapping and out-of-order ranges. Also,
;; extend the macro to handle singleton ranges written as a single numeric
;; constant.

;(define-syntax-rule (integer-ranges-predicate-v4 [start end] ...)
;  (lambda (x)
;    (integer-ranges-predicate-v4* #f x [start end] ...)))
#|
(define-syntax (integer-ranges-predicate-v4 stx)
  (syntax-case stx ()
    [(_ range ...)
     (let* ([ranges (syntax->list #'(range ...))]
            [new-ranges (map (lambda (r)
                               (syntax-case r ()
                                 [num         #'[num num]]
                                 [[start end] #'[start end]])) ranges)])
       #`(lambda (x) (integer-ranges-predicate-v4* #f x #,@new-ranges)))]))

(define-syntax (integer-ranges-predicate-v4* stx)
  (syntax-case stx ()
    [(_ if-tree x) #'if-tree]
    [(_ if-tree x [start end] ...)
     (let* ([ranges (syntax->list #'([start end] ...))]
            [rest (cdr ranges)])
       (with-syntax ([[s e] (car ranges)])
         (letrec
             ([cons-if-tree
               (lambda (if-t)
                 (let* ([if-t-e (syntax-e if-t)]
                        [new-if #'(if (<= s x e) #t (if (< x s) #f #f))])
                   (if (list? if-t-e)
                       (let*-values ([(if-stx cond-stx then-stx else-stx) (apply values if-t-e)]
                                     [(s*) (eval-syntax (cadr (syntax-e cond-stx)))]
                                     [(else-stx-e) (syntax-e else-stx)]
                                     [(new-else-stx) (if (list? else-stx-e)
                                                      (let-values ([(if-stx* cond-stx* then-stx* else-stx*)
                                                                    (apply values else-stx-e)])
                                                        (if (< (eval-syntax #'e) s*)
                                                            #`(if #,cond-stx* #,(cons-if-tree then-stx*) #,else-stx*)
                                                            #`(if #,cond-stx* #,then-stx* #,(cons-if-tree else-stx*))))
                                                      new-if)])
                         #`(if #,cond-stx #,then-stx #,new-else-stx))
                       new-if)))])
           #`(integer-ranges-predicate-v4* #,(cons-if-tree #'if-tree) x #,@rest))))]))

(define small-composite1-v4?
  (integer-ranges-predicate-v4
   [6 6] [4 4] [9 12] [8 10]))

(module+ test
  (check-equal? (small-composite1-v4? 12) (small-composite? 12))
  (check-equal? (small-composite1-v4? 6) (small-composite? 6))
  (check-equal? (small-composite1-v4? 5) (small-composite? 5)))
|#
#|
(define small-composite2-v4?
  (integer-ranges-predicate-v4
   [4 4] [6 6] [8 10] [12 12] [14 16] [18 18] [20 22] [24 28]))


(module+ test
  (require (only-in math/number-theory prime?))
  (for ([k (in-range 2 30)])
    (test-case (format "number ~s" k)
      (check-equal? (small-composite2-v4? k) (not (prime? k))))))

(define ascii-alphanum1-v4?
  (integer-ranges-predicate-v4
   [48 57] [65 90] [97 122]))

(module+ test
  (for ([k (in-range 128)])
    (test-case (format "character ~s" k)
      (check-equal? (ascii-alphanum1-v4? k)
                    (or (char-alphabetic? (integer->char k))
                        (char-numeric? (integer->char k)))))))

(define ascii-alphanum2-v4?
  (integer-ranges-predicate-v4
   [48 57] [97 122] [65 90]))

(define ascii-alphanum3-v4?
  (integer-ranges-predicate-v4
   [48 57] [97 122] [50 90]))

(define ascii-alphanum2-v3?
  (integer-ranges-predicate-v3
   [48 65] [65 90] [97 122]))

(module+ test
  (for ([k (in-range 128)])
    (test-case (format "character ~s" k)
      (check-equal? (ascii-alphanum2-v4? k)
                    (or (char-alphabetic? (integer->char k))
                        (char-numeric? (integer->char k)))))))

(module+ test
  (for ([k (in-range 128)])
    (test-case (format "character ~s" k)
      (check-equal? (ascii-alphanum3-v4? k)
                    (ascii-alphanum2-v3? k)))))
|#

;; Examples:

(define unassigned-in-unicode-3.2?
  (integer-ranges-predicate-v3
   #x0221
   [#x0234 #x024F]
   [#x02AE #x02AF]
   [#x02EF #x02FF]
   [#x0350 #x035F]
   [#x0370 #x0373]
   [#x0376 #x0379]
   [#x037B #x037D]
   [#x037F #x0383]
   #x038B
   #x038D
   #x03A2
   #x03CF
   [#x03F7 #x03FF]
   #x0487
   #x04CF
   [#x04F6 #x04F7]
   [#x04FA #x04FF]
   [#x0510 #x0530]
   [#x0557 #x0558]
   #x0560
   #x0588
   [#x058B #x0590]
   #x05A2
   #x05BA
   [#x05C5 #x05CF]
   [#x05EB #x05EF]
   [#x05F5 #x060B]
   [#x060D #x061A]
   [#x061C #x061E]
   #x0620
   [#x063B #x063F]
   [#x0656 #x065F]
   [#x06EE #x06EF]
   #x06FF
   #x070E
   [#x072D #x072F]
   [#x074B #x077F]
   [#x07B2 #x0900]
   #x0904
   [#x093A #x093B]
   [#x094E #x094F]
   [#x0955 #x0957]
   [#x0971 #x0980]
   #x0984
   [#x098D #x098E]
   [#x0991 #x0992]
   #x09A9
   #x09B1
   [#x09B3 #x09B5]
   [#x09BA #x09BB]
   #x09BD
   [#x09C5 #x09C6]
   [#x09C9 #x09CA]
   [#x09CE #x09D6]
   [#x09D8 #x09DB]
   #x09DE
   [#x09E4 #x09E5]
   [#x09FB #x0A01]
   [#x0A03 #x0A04]
   [#x0A0B #x0A0E]
   [#x0A11 #x0A12]
   #x0A29
   #x0A31
   #x0A34
   #x0A37
   [#x0A3A #x0A3B]
   #x0A3D
   [#x0A43 #x0A46]
   [#x0A49 #x0A4A]
   [#x0A4E #x0A58]
   #x0A5D
   [#x0A5F #x0A65]
   [#x0A75 #x0A80]
   #x0A84
   #x0A8C
   #x0A8E
   #x0A92
   #x0AA9
   #x0AB1
   #x0AB4
   [#x0ABA #x0ABB]
   #x0AC6
   #x0ACA
   [#x0ACE #x0ACF]
   [#x0AD1 #x0ADF]
   [#x0AE1 #x0AE5]
   [#x0AF0 #x0B00]
   #x0B04
   [#x0B0D #x0B0E]
   [#x0B11 #x0B12]
   #x0B29
   #x0B31
   [#x0B34 #x0B35]
   [#x0B3A #x0B3B]
   [#x0B44 #x0B46]
   [#x0B49 #x0B4A]
   [#x0B4E #x0B55]
   [#x0B58 #x0B5B]
   #x0B5E
   [#x0B62 #x0B65]
   [#x0B71 #x0B81]
   #x0B84
   [#x0B8B #x0B8D]
   #x0B91
   [#x0B96 #x0B98]
   #x0B9B
   #x0B9D
   [#x0BA0 #x0BA2]
   [#x0BA5 #x0BA7]
   [#x0BAB #x0BAD]
   #x0BB6
   [#x0BBA #x0BBD]
   [#x0BC3 #x0BC5]
   #x0BC9
   [#x0BCE #x0BD6]
   [#x0BD8 #x0BE6]
   [#x0BF3 #x0C00]
   #x0C04
   #x0C0D
   #x0C11
   #x0C29
   #x0C34
   [#x0C3A #x0C3D]
   #x0C45
   #x0C49
   [#x0C4E #x0C54]
   [#x0C57 #x0C5F]
   [#x0C62 #x0C65]
   [#x0C70 #x0C81]
   #x0C84
   #x0C8D
   #x0C91
   #x0CA9
   #x0CB4
   [#x0CBA #x0CBD]
   #x0CC5
   #x0CC9
   [#x0CCE #x0CD4]
   #x0CD7 #x0CDD
   #x0CDF
   [#x0CE2 #x0CE5]
   [#x0CF0 #x0D01]
   #x0D04
   #x0D0D
   #x0D11
   #x0D29
   [#x0D3A #x0D3D]
   [#x0D44 #x0D45]
   #x0D49
   [#x0D4E #x0D56]
   [#x0D58 #x0D5F]
   [#x0D62 #x0D65]
   [#x0D70 #x0D81]
   #x0D84
   [#x0D97 #x0D99]
   #x0DB2
   #x0DBC
   [#x0DBE #x0DBF]
   [#x0DC7 #x0DC9]
   [#x0DCB #x0DCE]
   #x0DD5
   #x0DD7
   [#x0DE0 #x0DF1]
   [#x0DF5 #x0E00]
   [#x0E3B #x0E3E]
   [#x0E5C #x0E80]
   #x0E83
   [#x0E85 #x0E86]
   #x0E89
   [#x0E8B #x0E8C]
   [#x0E8E #x0E93]
   #x0E98
   #x0EA0
   #x0EA4
   #x0EA6
   [#x0EA8 #x0EA9]
   #x0EAC
   #x0EBA
   [#x0EBE #x0EBF]
   #x0EC5
   #x0EC7
   [#x0ECE #x0ECF]
   [#x0EDA #x0EDB]
   [#x0EDE #x0EFF]
   #x0F48
   [#x0F6B #x0F70]
   [#x0F8C #x0F8F]
   #x0F98
   #x0FBD
   [#x0FCD #x0FCE]
   [#x0FD0 #x0FFF]
   #x1022
   #x1028
   #x102B
   [#x1033 #x1035]
   [#x103A #x103F]
   [#x105A #x109F]
   [#x10C6 #x10CF]
   [#x10F9 #x10FA]
   [#x10FC #x10FF]
   [#x115A #x115E]
   [#x11A3 #x11A7]
   [#x11FA #x11FF]
   #x1207
   #x1247
   #x1249
   [#x124E #x124F]
   #x1257
   #x1259
   [#x125E #x125F]
   #x1287
   #x1289
   [#x128E #x128F]
   #x12AF
   #x12B1
   [#x12B6 #x12B7]
   #x12BF
   #x12C1
   [#x12C6 #x12C7]
   #x12CF
   #x12D7
   #x12EF
   #x130F
   #x1311
   [#x1316 #x1317]
   #x131F
   #x1347
   [#x135B #x1360]
   [#x137D #x139F]
   [#x13F5 #x1400]
   [#x1677 #x167F]
   [#x169D #x169F]
   [#x16F1 #x16FF]
   #x170D
   [#x1715 #x171F]
   [#x1737 #x173F]
   [#x1754 #x175F]
   #x176D
   #x1771
   [#x1774 #x177F]
   [#x17DD #x17DF]
   [#x17EA #x17FF]
   #x180F
   [#x181A #x181F]
   [#x1878 #x187F]
   [#x18AA #x1DFF]
   [#x1E9C #x1E9F]
   [#x1EFA #x1EFF]
   [#x1F16 #x1F17]
   [#x1F1E #x1F1F]
   [#x1F46 #x1F47]
   [#x1F4E #x1F4F]
   #x1F58
   #x1F5A
   #x1F5C
   #x1F5E
   [#x1F7E #x1F7F]
   #x1FB5
   #x1FC5
   [#x1FD4 #x1FD5]
   #x1FDC
   [#x1FF0 #x1FF1]
   #x1FF5
   #x1FFF
   [#x2053 #x2056]
   [#x2058 #x205E]
   [#x2064 #x2069]
   [#x2072 #x2073]
   [#x208F #x209F]
   [#x20B2 #x20CF]
   [#x20EB #x20FF]
   [#x213B #x213C]
   [#x214C #x2152]
   [#x2184 #x218F]
   [#x23CF #x23FF]
   [#x2427 #x243F]
   [#x244B #x245F]
   #x24FF
   [#x2614 #x2615]
   #x2618
   [#x267E #x267F]
   [#x268A #x2700]
   #x2705
   [#x270A #x270B]
   #x2728
   #x274C
   #x274E
   [#x2753 #x2755]
   #x2757
   [#x275F #x2760]
   [#x2795 #x2797]
   #x27B0
   [#x27BF #x27CF]
   [#x27EC #x27EF]
   [#x2B00 #x2E7F]
   #x2E9A
   [#x2EF4 #x2EFF]
   [#x2FD6 #x2FEF]
   [#x2FFC #x2FFF]
   #x3040
   [#x3097 #x3098]
   [#x3100 #x3104]
   [#x312D #x3130]
   #x318F
   [#x31B8 #x31EF]
   [#x321D #x321F]
   [#x3244 #x3250]
   [#x327C #x327E]
   [#x32CC #x32CF]
   #x32FF
   [#x3377 #x337A]
   [#x33DE #x33DF]
   #x33FF
   [#x4DB6 #x4DFF]
   [#x9FA6 #x9FFF]
   [#xA48D #xA48F]
   [#xA4C7 #xABFF]
   [#xD7A4 #xD7FF]
   [#xFA2E #xFA2F]
   [#xFA6B #xFAFF]
   [#xFB07 #xFB12]
   [#xFB18 #xFB1C]
   #xFB37
   #xFB3D
   #xFB3F
   #xFB42
   #xFB45
   [#xFBB2 #xFBD2]
   [#xFD40 #xFD4F]
   [#xFD90 #xFD91]
   [#xFDC8 #xFDCF]
   [#xFDFD #xFDFF]
   [#xFE10 #xFE1F]
   [#xFE24 #xFE2F]
   [#xFE47 #xFE48]
   #xFE53
   #xFE67
   [#xFE6C #xFE6F]
   #xFE75
   [#xFEFD #xFEFE]
   #xFF00
   [#xFFBF #xFFC1]
   [#xFFC8 #xFFC9]
   [#xFFD0 #xFFD1]
   [#xFFD8 #xFFD9]
   [#xFFDD #xFFDF]
   #xFFE7
   [#xFFEF #xFFF8]
   [#x10000 #x102FF]
   #x1031F
   [#x10324 #x1032F]
   [#x1034B #x103FF]
   [#x10426 #x10427]
   [#x1044E #x1CFFF]
   [#x1D0F6 #x1D0FF]
   [#x1D127 #x1D129]
   [#x1D1DE #x1D3FF]
   #x1D455
   #x1D49D
   [#x1D4A0 #x1D4A1]
   [#x1D4A3 #x1D4A4]
   [#x1D4A7 #x1D4A8]
   #x1D4AD
   #x1D4BA
   #x1D4BC
   #x1D4C1
   #x1D4C4
   #x1D506
   [#x1D50B #x1D50C]
   #x1D515
   #x1D51D
   #x1D53A
   #x1D53F
   #x1D545
   [#x1D547 #x1D549]
   #x1D551
   [#x1D6A4 #x1D6A7]
   [#x1D7CA #x1D7CD]
   [#x1D800 #x1FFFD]
   [#x2A6D7 #x2F7FF]
   [#x2FA1E #x2FFFD]
   [#x30000 #x3FFFD]
   [#x40000 #x4FFFD]
   [#x50000 #x5FFFD]
   [#x60000 #x6FFFD]
   [#x70000 #x7FFFD]
   [#x80000 #x8FFFD]
   [#x90000 #x9FFFD]
   [#xA0000 #xAFFFD]
   [#xB0000 #xBFFFD]
   [#xC0000 #xCFFFD]
   [#xD0000 #xDFFFD]
   #xE0000
   [#xE0002 #xE001F]
   [#xE0080 #xEFFFD]
   ))



;; ------------------------------------------------------------
;; v5: Add `define-integer-set` form, and allow integer set references as ranges.
;;
;; If the macro receives an identifier that was not defined using
;; `define-integer-set`, it should raise an error at compile time specifically
;; saying so.

#|
;; Examples:

(define-integer-set ascii-alpha [65 90] [97 122])
(define ascii-alphanum-v2?
  (integer-ranges-predicate [48 57] ascii-alphanum))

;; Tests
(for ([k (in-range 256)])
  (check-equal? (ascii-alphanum-v2? k)
                (or (char-alphabetic? (integer->char k))
                    (char-numeric? (integer->char k)))))
|#
