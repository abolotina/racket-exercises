#lang racket

(provide (all-defined-out))

; a closed interval of natural numbers
  (struct range (low high))

; sort-and-remove-overlaps : (list of range) -> (list of range)
(define (sort-and-remove-overlaps ranges)
  (letrec ([qsort
              (lambda (rs)
                (match rs
                  ['() '()]
                  [_ (let* ([h (car rs)]
                            [t (cdr rs)]
                            [lt-h? (lambda (x) (< (range-low x) (range-low h)))]
                            [ge-h? (lambda (x) (>= (range-low x) (range-low h)))])
                       (append (qsort (filter lt-h? t)) (list h) (qsort (filter ge-h? t))))]))]
             [sorted-rs (qsort ranges)]
             [len (length sorted-rs)]
             [last-el (list-ref sorted-rs (- len 1))]
             [unoverlap-sorted-rs (if (< len 2)
                                      sorted-rs
                                      (map (lambda (x y)
                                             (if (<= (range-high x) (range-low y))
                                                 x
                                                 (range (range-low x) (- (range-low y) 1))))
                                           sorted-rs
                                           (append (cdr sorted-rs) (list (range (range-high last-el)
                                                                                (range-high last-el))))))])
      unoverlap-sorted-rs))

;(define-syntax-rule (make-list-of-ranges [start end] ...)
;  (list (range start end) ...))

;(sort-and-remove-overlaps (list (range 6 6) (range 4 4) (range 9 12) (range 8 10)))
; =
;(sort-and-remove-overlaps (make-list-of-ranges
;[6 6] [4 4] [9 12] [8 10]
;))

