#lang racket

(require 
  raylib/2d/unsafe
  "stars.rkt"
  "player.rkt"
  "utils.rkt"
  "moon.rkt")

(define (main)
  (define screen-width 1200)
  (define screen-height 800)
  (InitWindow screen-width screen-height "Moonshot")
  (SetTargetFPS 60)
  (InitAudioDevice)

  (define FRAME (/ 1 60))
  (define dt FRAME)

  ;; -- Stars --
  (define num-stars (+ 50 (rand 0 200)))
  (define stars (create-stars num-stars screen-width screen-height))

  ;; -- Player --
  (define player-texture (LoadTexture "assets/graphics/shuttle.png"))
  (define bullet-texture (LoadTexture "assets/graphics/bullet.png"))
  (define player-shoot-sound (LoadSound "assets/sfx/Laser_Shoot.ogg"))
  (define player-got-hit (LoadSound "assets/sfx/Player_Hit.ogg"))
  (define player-rect (make-Rectangle 0.0 0.0 52.0 52.0))
  (define player (Player (make-Vector2 50.0 50.0) 0.0 500.0 0.0 null 0 (make-Rectangle 50.0 50.0 15.0 15.0) #f))
  (define player-hp 15.0)
  (SetSoundVolume player-shoot-sound 0.1)
  (SetSoundVolume player-got-hit 0.1)

  ;; -- Moon --
  (define moon-texture (LoadTexture "assets/graphics/spr_moon.png"))
  (define moon-bullet-texture (LoadTexture "assets/graphics/spacerock.png"))
  (define moon 
    (Moon 0.0 5.0 5.0 0.0 #t (make-Vector2 (/ screen-width 2.0) (/ screen-height 2.0)) 120.0 (initialize-boss-wave) null 0 #f 255.0))
  (define moon-hp 300.0)
  (define moon-shoot-sound (LoadSound "assets/sfx/Moon_Shoot.ogg"))
  (SetSoundVolume moon-shoot-sound 0.05)

  (for ([dummy (in-naturals)]
    #:break (WindowShouldClose))
    ;; -- Update --
    (set! dt (min FRAME (GetFrameTime)))
    (when (<= player-hp 0)
      ;; Reset
      (set! player-hp 15.0)
      (set! player (Player (make-Vector2 50.0 50.0) 0.0 500.0 0.0 null 0 (make-Rectangle 50.0 50.0 15.0 15.0) #f))
      (set! moon-hp 501.0)
      (set! moon 
        (Moon 0.0 5.0 5.0 0.0 #t (make-Vector2 (/ screen-width 2.0) (/ screen-height 2.0)) 120.0 (initialize-boss-wave) null 0 #f 255.0)))
    
    (when (positive? (Player-bullet-count player))
      (set! player (update-bullet player (Moon-position moon) (Moon-radius moon) dt))
      (when (Player-hit-boss player)
        (set! moon-hp (sub1 moon-hp))))
    (set! player (player-update player dt player-shoot-sound))
    (set! moon (update-moon moon dt moon-shoot-sound (Player-hitbox player) moon-hp))
    (when (Moon-hit-player moon)
      (PlaySound player-got-hit)
      (set! player-hp (sub1 player-hp)))

    ;; -- Draw --
    (BeginDrawing)
    (ClearBackground BLACK)
    (draw-stars stars num-stars)
    (draw-moon moon moon-texture moon-bullet-texture)
    (DrawTexturePro player-texture player-rect
      (make-Rectangle (Vector2-x (Player-position player)) (Vector2-y (Player-position player)) 52.0 52.0)
      (make-Vector2 26.0 26.0) (radians->degrees (Player-angle player)) (if (Moon-hit-player moon) RED WHITE))
    (when (positive? (Player-bullet-count player))
      (draw-bullet player bullet-texture (Player-bullet-count player)))
    (DrawText "MOON" 10 10 10 RED)
    (DrawRectangleRec (make-Rectangle 60.0 10.0 moon-hp 10.0) (make-Color 120 0 0 150))
    (DrawText "SPACESHIP" 10 (- (GetScreenHeight) 20) 10 GREEN)
    (DrawRectangleRec (make-Rectangle 90.0 (- (GetScreenHeight) 20.0) (* player-hp 2) 10.0) (make-Color 0 120 0 150))
    (DrawFPS (- (GetScreenWidth) 90) 0)
    (EndDrawing))
  ;; Unload and close
  (UnloadTexture player-texture)
  (UnloadTexture bullet-texture)
  (UnloadTexture moon-texture)
  (UnloadTexture moon-bullet-texture)
  (UnloadSound player-shoot-sound)
  (UnloadSound moon-shoot-sound)
  (UnloadSound player-got-hit)
  (CloseAudioDevice)
  (CloseWindow))

(main)