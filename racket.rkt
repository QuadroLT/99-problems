#lang racket

(require rackunit rackunit/text-ui)

;; P01 get last element in the list

(define (last-element lst)
  (cond
    [(empty? lst) null]
    [(empty? (cdr lst)) (car lst)]
    [else (last-element (cdr lst))]
    )
  )

;; P02 get second to last element in the list (aligning to haskell's interpretation)


(define (second-to-last lst)
  (cond
    [(empty? lst) null]
    [(empty? (cdr lst)) null]
    [(empty? (cdr (cdr lst))) (car lst)]
    [else (second-to-last (cdr lst))]
    )
  )

;; P03 get n-th element of the list

(define (nth-element n lst)
  (cond
    [(= n 0) null]
    [(empty? lst) null]
    [(= n 1) (car lst)]
    [else (nth-element (- n 1) (cdr lst))]
    )
  )

;; P04 get length of a list

(define (list-length lst)
  (define (inner acc l)
    (cond
      [(empty? l) acc]
      [else (inner (+ 1 acc) (cdr l))]))
  (inner 0 lst))


;; P05 reverse a list

(define (reverse-list lst)
  (define (inner acc l)
    (cond
      [(empty? l) acc]
      [else (inner (cons (car l) acc) (cdr l))]))
  (inner '() lst))

;; Tests

(define test-01
  (test-suite "P01"
              (check-equal? (last-element '(1 2 3)) 3 "Regular list")
              (check-equal? (last-element '(1)) 1 "Single element list")
              (check-equal? (last-element '()) null "Empty list")))

(define test-02
  (test-suite "P02"
              (check-equal? (second-to-last '(1 2 3)) 2 "Regular list")
              (check-equal? (second-to-last '(1)) null "Single element list")
              (check-equal? (second-to-last '()) null "Empty list")))

(define test-03
  (test-suite "P03"
              (check-equal? (nth-element 3 '(1 2 3)) 3 "Regular list")
              (check-equal? (nth-element 0 '(1 2 3)) null "invalid index")
              (check-equal? (nth-element 3 '()) null "empty list with any index")))

(define test-04
  (test-suite "P04"
              (check-equal? (list-length '(1, 2, 3)) 3 "Regular list")
              (check-equal? (list-length '()) 0 "Empty list")
              ))

(define test-05
  (test-suite "P05"
              (check-equal? (reverse-list '(1 2 3)) '(3 2 1) "regular list")
              ))
;; Runner

(define (main)
  (displayln "Running tests")
  (run-tests test-01)
  (run-tests test-02)
  (run-tests test-03)
  (run-tests test-04)
  (run-tests test-05)
  (displayln "Done"))

(main)
