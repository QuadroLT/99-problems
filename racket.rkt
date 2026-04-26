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
              (check-equal? (nth-element 3 '()) null "empty list with any index")
              ))


;; Runner

(define (main)
  (displayln "Running tests")
  (run-tests test-01)
  (run-tests test-02)
  (run-tests test-03)
  (displayln "Done"))

(main)
