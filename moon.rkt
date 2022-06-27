#lang racket

(provide
  (struct-out BossWave)
  (struct-out Moon)
  initialize-boss-wave
  update-moon
  draw-moon)

(require
  raylib/2d/unsafe
  rebellion/type/enum
  "bullet.rkt"
  "utils.rkt")

(define-enum-type WaveType (Start Circular Rotational Line CutLine RotationalDec StraightPlayer Random))

;; Defines the order of the waves
(define/contract (WaveType->next wave)
  (-> WaveType? WaveType?)
  (match wave
    [(== Start) Circular]
    [(== Circular) Rotational]
    [(== Rotational) Line]
    [(== Line) CutLine]
    [(== CutLine) RotationalDec]
    [(== RotationalDec) StraightPlayer]
    [(== StraightPlayer) Random]
    [else wave]))

(struct BossWave (current delay max-delay wave-delay max-wave-delay time-new-wave timer-wave can-shoot started random-wave acc) #:mutable)
(struct Moon (index delay max-delay angle index-up position radius boss-wave bullet-list bullet-created hit-player size) #:mutable)

(define angle-change-rate (/ pi 100))
(define MAX-INDEX 4)
(define count 0.0)
(define speed 0.0)
(define bullet-max-count 32)
(define wave-list (list Circular Rotational Line CutLine RotationalDec StraightPlayer))
(define game-over #f)

(define (initialize-boss-wave)
  (BossWave Start 0.0 0.0 0.0 0.0 0.0 3.0 #f #f #f 0.0))

(define (set-waves moon)
  (set-BossWave-delay! (Moon-boss-wave moon) 1.0)
  (set-BossWave-max-delay! (Moon-boss-wave moon) 1.0)
  (set-BossWave-time-new-wave! (Moon-boss-wave moon) 15.0)
  (set-BossWave-wave-delay! (Moon-boss-wave moon) 0.5)
  (set-BossWave-max-wave-delay! (Moon-boss-wave moon) 0.5)
  (set! speed 130.0)
  (set-BossWave-acc! (Moon-boss-wave moon) 0.0)

  ;; Set variables based on the current wave
  (match (BossWave-current (Moon-boss-wave moon))
    [(or (== Rotational) (== RotationalDec))
      (set-BossWave-delay! (Moon-boss-wave moon) 0.5)
      (set-BossWave-max-delay! (Moon-boss-wave moon) 0.5)
      (set-BossWave-time-new-wave! (Moon-boss-wave moon) 20.0)
      (if (equal? RotationalDec (BossWave-current (Moon-boss-wave moon)))
        (set-BossWave-acc! (Moon-boss-wave moon) -0.2)
        (set-BossWave-acc! (Moon-boss-wave moon) 0.0))]
    [(or (== Line) (== CutLine))
      (set-BossWave-delay! (Moon-boss-wave moon) 0.2)
      (set-BossWave-max-delay! (Moon-boss-wave moon) 0.2)
      (set-BossWave-time-new-wave! (Moon-boss-wave moon) 25.0)]
    [(== StraightPlayer)
      (set-BossWave-delay! (Moon-boss-wave moon) 0.15)
      (set-BossWave-max-delay! (Moon-boss-wave moon) 0.15)
      (set-BossWave-time-new-wave! (Moon-boss-wave moon) 25.0)
      (set! speed 180.0)]
    [else (void)])
  (set-BossWave-can-shoot! (Moon-boss-wave moon) #t)
  moon)

(define (set-bullets moon dt bullet-sound player-hitbox)
  (set-BossWave-delay! (Moon-boss-wave moon) (- (BossWave-delay (Moon-boss-wave moon)) dt))
  (when (and (<= (BossWave-delay (Moon-boss-wave moon)) 0.0) (positive? (BossWave-time-new-wave (Moon-boss-wave moon))))
    (set-BossWave-delay! (Moon-boss-wave moon) (BossWave-max-delay (Moon-boss-wave moon)))
    (PlaySound bullet-sound)
    (match (BossWave-current (Moon-boss-wave moon))
      [(== Circular)
        (for ([i (* bullet-max-count 2)])
          (set-Moon-bullet-list! moon 
            (add-end (Moon-bullet-list moon) (Bullet
              (make-Vector2 (Vector2-x (Moon-position moon)) (Vector2-y (Moon-position moon)))
              (exact->inexact (* i (/ pi 16.0)))
              (make-Rectangle (Vector2-x (Moon-position moon)) (Vector2-y (Moon-position moon)) 5.0 5.0)
              speed
              (BossWave-acc (Moon-boss-wave moon)))))
          (set-Moon-bullet-created! moon (add1 (Moon-bullet-created moon))))]
      [(or (== Rotational) (== Line) (== CutLine) (== RotationalDec))
        (if (or (equal? Line (BossWave-current (Moon-boss-wave moon))) (equal? CutLine (BossWave-current (Moon-boss-wave moon))))
          (set! count (+ count (* 2.0 dt)))
          (set! count (add1 count)))
        (let ([stop-loop #f])
          (for ([i bullet-max-count] #:break stop-loop)
            (when (and (equal? CutLine (BossWave-current (Moon-boss-wave moon))) (< (random 0 100) 10))
              (set! stop-loop #t))
            (set-Moon-bullet-list! moon 
              (add-end (Moon-bullet-list moon) (Bullet
                (make-Vector2 (Vector2-x (Moon-position moon)) (Vector2-y (Moon-position moon)))
                (exact->inexact (+ (* i (/ pi 4.0)) count))
                (make-Rectangle (Vector2-x (Moon-position moon)) (Vector2-y (Moon-position moon)) 5.0 5.0)
                speed
                (BossWave-acc (Moon-boss-wave moon)))))
          (set-Moon-bullet-created! moon (add1 (Moon-bullet-created moon)))))]
      [(== StraightPlayer)
        (let*
          ([y-diff (- (Rectangle-y player-hitbox) (Vector2-y (Moon-position moon)))]
          [x-diff (- (Rectangle-x player-hitbox) (Vector2-x (Moon-position moon)))]
          [angle-moon-player (atan2 y-diff x-diff)])
            (for ([i (in-range -1 5)])
              (set-Moon-bullet-list! moon
                (add-end (Moon-bullet-list moon) (Bullet
                  (make-Vector2 (Vector2-x (Moon-position moon)) (Vector2-y (Moon-position moon)))
                  (+ angle-moon-player (* i (/ pi 10)) (if (> i 1) (/ (* 3 pi) 4) 0.0))
                  (make-Rectangle (Vector2-x (Moon-position moon)) (Vector2-y (Moon-position moon)) 5.0 5.0)
                  speed
                  (BossWave-acc (Moon-boss-wave moon)))))
              (set-Moon-bullet-created! moon (add1 (Moon-bullet-created moon)))))]))
    (if (<= (BossWave-time-new-wave (Moon-boss-wave moon)) 0.0)
      (begin
        (set-BossWave-can-shoot! (Moon-boss-wave moon) #f)
        (set-BossWave-started! (Moon-boss-wave moon) #f)
        (set-BossWave-timer-wave! (Moon-boss-wave moon) (+ 3.0 (random 0 5))))
      (set-BossWave-time-new-wave! (Moon-boss-wave moon) (- (BossWave-time-new-wave (Moon-boss-wave moon)) dt)))
  moon)

(define (update-bullets moon dt player-hitbox)
  (set-Moon-hit-player! moon #f)
  (for ([bullet (Moon-bullet-list moon)])
    (set! bullet (move-bullet bullet dt))
    (when (CheckCollisionRecs (Bullet-hitbox bullet) player-hitbox)
      (set-Moon-hit-player! moon #t)
      (set-Moon-bullet-list! moon (remove bullet (Moon-bullet-list moon)))
      (set-Moon-bullet-created! moon (sub1 (Moon-bullet-created moon))))
    (when (is-offscreen (Vector2-x (Bullet-position bullet)) (Vector2-y (Bullet-position bullet)) 290)
      (set-Moon-bullet-list! moon (remove bullet (Moon-bullet-list moon)))
      (set-Moon-bullet-created! moon (sub1 (Moon-bullet-created moon)))))
  moon)

(define (update-moon moon dt bullet-sound player-hitbox moon-hp)
  (set! game-over (<= moon-hp 0))

  (when game-over
    (set-BossWave-can-shoot! (Moon-boss-wave moon) #f)
    (set! angle-change-rate 10.0)
    (when (positive? (Moon-size moon))
      (set-Moon-size! moon (- (Moon-size moon) 1.0))))

  (when (not (BossWave-started (Moon-boss-wave moon)))
    (set-BossWave-timer-wave! (Moon-boss-wave moon) (- (BossWave-timer-wave (Moon-boss-wave moon)) dt))
    (when (and (<= (BossWave-timer-wave (Moon-boss-wave moon)) 0))
      (set! count 0.0)
      (set-BossWave-started! (Moon-boss-wave moon) #t)
      (set-BossWave-current! (Moon-boss-wave moon) (WaveType->next (BossWave-current (Moon-boss-wave moon))))
      ;; Random wave
      (when (or (equal? (BossWave-current (Moon-boss-wave moon)) Random) (BossWave-random-wave (Moon-boss-wave moon)))
        (set-BossWave-random-wave! (Moon-boss-wave moon) #t)
        (set-BossWave-current! (Moon-boss-wave moon) (get-list-value wave-list (rand 0 (sub1 (length wave-list))))))
      (set! moon (set-waves moon))))
  (when (BossWave-can-shoot (Moon-boss-wave moon))
    (set! moon (set-bullets moon dt bullet-sound player-hitbox)))

  (set! moon (update-bullets moon dt player-hitbox))

  (set-Moon-angle! moon (+ (Moon-angle moon) (* angle-change-rate dt)))
  (set-Moon-delay! moon (- (Moon-delay moon) dt))
  
  ;; Animation update
  (when (<= (Moon-delay moon) 0.0)
    (set-Moon-delay! moon (Moon-max-delay moon))
    (set-Moon-index! moon
      (if (Moon-index-up moon) 
        (add1 (Moon-index moon))
        (sub1 (Moon-index moon))))
    (when (or (= (Moon-index moon) MAX-INDEX) (zero? (Moon-index moon)))
      (set-Moon-index-up! moon (not (Moon-index-up moon)))))
  moon)

(define (draw-moon moon texture bullet-texture)
  (DrawTexturePro
    texture
    (make-Rectangle (* (Moon-index moon) 255.0) 0.0 255.0 255.0)
    (make-Rectangle (Vector2-x (Moon-position moon)) (Vector2-y (Moon-position moon)) (Moon-size moon) (Moon-size moon))
    (make-Vector2 127.5 127.5)
    (radians->degrees (Moon-angle moon))
    WHITE)
  ;;(DrawCircleV (Moon-position moon) (Moon-radius moon) WHITE)
  (when (positive? (Moon-bullet-created moon))
    (for ([bullet (Moon-bullet-list moon)])
      (DrawTexturePro
        bullet-texture
        (make-Rectangle 0.0 0.0 15.0 15.0)
        (make-Rectangle (Vector2-x (Bullet-position bullet)) (Vector2-y (Bullet-position bullet)) 15.0 15.0)
        (make-Vector2 7.5 7.5)
        (radians->degrees (Bullet-angle bullet))
        WHITE))))
      ;;(DrawRectangleRec (Bullet-hitbox bullet) WHITE))))

