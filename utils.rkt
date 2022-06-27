#lang racket

(provide
  add-end
  get-list-value
  rand
  atan2)

;; Add new element to the end of the list
;; append joins two lists, so we just
;; create a new list with the value
(define (add-end my-list value)
  (append my-list (list value)))

;; Returns the nth element of a list
;; If the list is empty return an empty list
;; If the index is zero, return the first element
;; otherwise remove the first element, decrease index by one
;; and call the function again
(define (get-list-value my-list index)
  (cond 
    ((empty? my-list) '())
    ((equal? index 0) (first my-list))
    (else (get-list-value (rest my-list) (sub1 index)))))

;; Returns a random value between min and max inclusive
(define (rand min max)
  (+ (random (add1 (- max min))) min))

;; Returns the angle between two points
(define (atan2 y x)
  (cond
    [(positive? x)
      (atan (/ y x))]
    [(positive? y)
      (- (/ pi 2) (atan (/ x y)))]
    [(negative? y)
      (- (- (/ pi 2)) (atan (/ x y)))]
    [(negative? x)
      (+ atan (/ y x) pi)]
    [else 0.0]))
