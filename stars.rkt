#lang racket

(provide
  Star
  create-stars
  draw-stars)

(require
  raylib/2d/unsafe
  "utils.rkt")

;; -- Star --
(struct Star (x1 y1 x2 y2))

(define (create-stars num-stars screen-width screen-height)
  (let ([stars '()])
    (for ([i num-stars])
      (let* ([x1 (rand 0 screen-width)]
             [y1 (rand 0 screen-height)]
             [x2 (+ x1  (rand 0 2))]
             [y2 (+ y1 (rand 0 2))])
                (set! stars (add-end stars (Star x1 y1 x2 y2)))))
  stars))

(define (draw-stars stars num-stars)
  (for ([i num-stars])
    (let ([current-star (get-list-value stars i)] [color (make-Color #x66 #x66 #x66 #xFF)])
      (DrawLine (Star-x1 current-star) (Star-y1 current-star) (Star-x2 current-star) (Star-y2 current-star) color))))
