#lang racket

(provide
  (struct-out Player)
  player-update
  update-bullet
  draw-bullet
  )

(require
  raylib/2d/unsafe
  "bullet.rkt"
  "utils.rkt")

(struct Player (position speed max-speed angle bullet-list bullet-count hitbox hit-boss) #:mutable)

(define SCREEN-OFFSET 40.0)
(define MAX-BULLET-DELAY 0.3)
(define BULLET-SPEED 550)
(define PLAYER-ACC 300)
(define PLAYER-DECEL 200)
(define bullet-delay 0.0)

(define (player-update player dt bullet-sound)
  (when (positive? bullet-delay)
    (set! bullet-delay (- bullet-delay dt)))
  (when (and (IsKeyDown KEY_SPACE) (<= bullet-delay 0.0))
    (set-Player-bullet-list!
      player
      (let ([x (Vector2-x (Player-position player))] [y (Vector2-y (Player-position player))])
        (add-end (Player-bullet-list player) (Bullet (make-Vector2 x y) (Player-angle player) (make-Rectangle x y 10.0 10.0) BULLET-SPEED 0.0))))
    (set-Player-bullet-count! player (add1 (Player-bullet-count player)))
    (set! bullet-delay MAX-BULLET-DELAY)
    (PlaySound bullet-sound))
  (cond
    [(IsKeyDown KEY_A)
      (set-Player-angle! player (- (Player-angle player) (* 6 dt)))]
    [(IsKeyDown KEY_D)
      (set-Player-angle! player (+ (Player-angle player) (* 6 dt)))])
  (cond
    [(IsKeyDown KEY_W)
      (set-Player-speed! player
        (if (< (Player-speed player) (Player-max-speed player))
          (+ (Player-speed player) (* PLAYER-ACC dt))
          (Player-speed player)))]
    [(IsKeyDown KEY_S)
      (set-Player-speed! player
        (if (> (Player-speed player) (- (Player-max-speed player)))
          (- (Player-speed player) (* PLAYER-ACC dt))
          (Player-speed player)))]
    [else 
      (set-Player-speed! player
        (cond
          [(positive? (Player-speed player)) (- (Player-speed player) (* PLAYER-DECEL dt))]
          [(negative? (Player-speed player)) (+ (Player-speed player) (* PLAYER-DECEL dt))]
          [else (Player-speed player)]))])
  (set-Vector2-x! (Player-position player) (+ (Vector2-x (Player-position player)) (* (Player-speed player) (cos (Player-angle player)) dt)))
  (set-Vector2-y! (Player-position player) (+ (Vector2-y (Player-position player)) (* (Player-speed player) (sin (Player-angle player)) dt)))

  (set-Vector2-x! (Player-position player)
    (cond 
      [(> (Vector2-x (Player-position player)) (+ (GetScreenWidth) SCREEN-OFFSET)) (- SCREEN-OFFSET)]
      [(< (Vector2-x (Player-position player)) (- SCREEN-OFFSET)) (+ (GetScreenWidth) SCREEN-OFFSET)]
      [else (Vector2-x (Player-position player))]))

    (set-Vector2-y! (Player-position player)
      (cond 
        [(> (Vector2-y (Player-position player)) (+ (GetScreenHeight) SCREEN-OFFSET)) (- SCREEN-OFFSET)]
        [(< (Vector2-y (Player-position player)) (- SCREEN-OFFSET)) (+ (GetScreenHeight) SCREEN-OFFSET)]
        [else (Vector2-y (Player-position player))]))
  (set-Rectangle-x! (Player-hitbox player) (- (Vector2-x (Player-position player)) 10))
  (set-Rectangle-y! (Player-hitbox player) (- (Vector2-y (Player-position player)) 10))

  player)

(define (update-bullet player moon-pos moon-radius dt)
  (set-Player-hit-boss! player #f)
  (for ([bullet (Player-bullet-list player)])
    (set! bullet (move-bullet bullet dt))
    (when (CheckCollisionCircleRec moon-pos moon-radius (Bullet-hitbox bullet))
      (set-Player-hit-boss! player #t)
      (set-Player-bullet-list! player (remove bullet (Player-bullet-list player)))
      (set-Player-bullet-count! player (sub1 (Player-bullet-count player))))
    (when (is-offscreen (Vector2-x (Bullet-position bullet)) (Vector2-y (Bullet-position bullet)) 0)
      (set-Player-bullet-list! player (remove bullet (Player-bullet-list player)))
      (set-Player-bullet-count! player (sub1 (Player-bullet-count player)))))
  player)

(define (draw-bullet player texture bullet-count)
  (for ([bullet (Player-bullet-list player)])
      (DrawTexturePro
        texture
        (make-Rectangle 0.0 0.0 16.0 16.0)
        (make-Rectangle (Vector2-x (Bullet-position bullet)) (Vector2-y (Bullet-position bullet)) 16.0 16.0)
        (make-Vector2 8.0 8.0)
        (radians->degrees (Bullet-angle bullet))
        WHITE)))
      ;(DrawRectangleRec (Bullet-hitbox bullet) WHITE))))
