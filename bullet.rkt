#lang racket

(provide
  (struct-out Bullet)
  is-offscreen
  move-bullet)

(require
  raylib/2d/unsafe)

(struct Bullet (position angle hitbox speed acc) #:mutable)

(define (is-offscreen x y offset)
  (or (> x (+ (GetScreenWidth)) offset) (< x (- offset)) (> y (+ (GetScreenHeight) offset)) (< y (- offset))))

(define (move-bullet bullet dt)
  (set-Bullet-speed! bullet (+ (Bullet-speed bullet) (Bullet-acc bullet)))
  (set-Vector2-x! (Bullet-position bullet) (+ (Vector2-x (Bullet-position bullet)) (* (Bullet-speed bullet) (cos (Bullet-angle bullet)) dt)))
  (set-Vector2-y! (Bullet-position bullet) (+ (Vector2-y (Bullet-position bullet)) (* (Bullet-speed bullet) (sin (Bullet-angle bullet)) dt)))
  (set-Rectangle-x! (Bullet-hitbox bullet) (Vector2-x (Bullet-position bullet)))
  (set-Rectangle-y! (Bullet-hitbox bullet) (Vector2-y (Bullet-position bullet)))
  bullet)