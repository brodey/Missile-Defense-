;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname missile_defense) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)

;-------------------------------------------------------------------------------
; DATA DEFINITIONS & TEMPLATES
;-------------------------------------------------------------------------------

;; A SPRITE is a (make-sprite Posn Posn Number Symbol) where:
;; - LOC is the sprite's location
;; - VEL is its velocity
;; - SIZE is the radius of the sprite
;; - COLOR is its color.
;; Location, velocity and size are in computer-graphic/pixel coordinates.
;; A sprite represents either an attacker's missile or a defender's
;; anti-missile bullet.
(define-struct sprite (loc vel size color))
#;
(define (sprite-template sprite)
  ... (sprite-loc sprite) ...
  ... (sprite-vel sprite) ...
  ... (sprite-size sprite) ...
  ... (sprite-color sprite) ...)

;; A LOS (list of sprites) is one of:
;; - empty
;; - (cons SPRITE LOS)

;; A WORLD structure is a (make-world LOS LOS Number) where:
;; - MISSILES: the missiles attacking the player
;; - BULLETS: the missiles launched by the player
;; - HEALTH: current health of the player -- game-over if health <= 0
(define-struct world (missiles bullets health))
#;
(define (world-template world)
  ... (world-missiles world) ...
  ... (world-bullets world)...
  ... (world-health world)...)


;-------------------------------------------------------------------------------
; CONSTANTS
;-------------------------------------------------------------------------------

(define world-width 500)
(define world-height 500)
(define gun-size 10)
(define gun-position (make-posn (/ world-width 2) (- (- world-height (/ gun-size 2)) 1)))
(define gun-color 'green)
(define gun (square gun-size 'solid gun-color))
(define missile-vel (make-posn 0 5))
(define missile-r 10)
(define missile-color 'red)
(define missile (circle missile-r 'solid missile-color))
(define bullet-r 5)
(define bullet-color 'green)
(define bullet (circle bullet-r 'solid bullet-color))
(define missile-damage 1)
(define max-missiles 10)


;-------------------------------------------------------------------------------
; HELPER FUNCTIONS
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;; on-screen? : SPRITE -> Boolean
;; Given a sprite, produces a boolean--true if the sprite is on screen,
;; false if it is not.

; For testing
(define m1 (make-sprite (make-posn 100 100) 
                        (make-posn -3 2) 
                        missile-r 
                        missile-color))
(define m2 (make-sprite (make-posn 50 50) 
                        (make-posn 1 1)
                        missile-r 
                        missile-color))
(define m3 (make-sprite (make-posn 50 500) 
                        (make-posn 1 0) 
                        missile-r 
                        missile-color))
(define b1 (make-sprite (make-posn -50 50) 
                        (make-posn -5 5) 
                        bullet-r 
                        bullet-color))
(define b2 (make-sprite (make-posn 45 50) 
                        (make-posn 2 3) 
                        bullet-r 
                        bullet-color))

(check-expect (on-screen? m1) true)
(check-expect (on-screen? b1) false)

(define (on-screen? sprite)
  (cond [(and (and (< 0 (posn-x (sprite-loc sprite))) 
                   (< (posn-x (sprite-loc sprite)) world-width))
              (and (< 0 (posn-y (sprite-loc sprite)))
                   (< (posn-x (sprite-loc sprite)) world-height))) true]
        [else false]))


;-------------------------------------------------------------------------------
;; collision? : SPRITE SPRITE -> Boolean
;; Given two sprites, produces a boolean--true if the distance between
;; the two centers is less than the sum of their radii (they collided),
;; false if not.

(check-expect (collision? m1 b1) false)
(check-expect (collision? m2 b2) true)

(define (collision? sprite1 sprite2)
  (cond [(< (sqrt (+ (sqr (- (posn-x (sprite-loc sprite1)) 
                             (posn-x (sprite-loc sprite2)))) 
                     (sqr (- (posn-y (sprite-loc sprite1)) 
                             (posn-y (sprite-loc sprite2)))))) 
            (+ (sprite-size sprite1) 
               (sprite-size sprite2))) true]
        [else false]))


;-------------------------------------------------------------------------------
;; list-length : LOS -> Number
;; Given a LOS, produces how many sprites are in the list

; For testing
(define los$ (list (make-sprite (make-posn 1 2) 
                                (make-posn 1 1) 
                                missile-r 
                                missile-color)
                   (make-sprite (make-posn 2 3)
                                (make-posn 1 1)
                                missile-r
                                missile-color)
                   (make-sprite (make-posn 3 4)
                                (make-posn 1 1)
                                missile-r
                                missile-color)))

(check-expect (list-length los$) 3)

(define (list-length list)
  (cond [(empty? list) 0]
        [else (+ 1 (list-length (rest list)))]))


;-------------------------------------------------------------------------------
; TOP-LEVEL FUNCTIONS
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;; move-bullets : World -> World
;; Move the bullets one time step and remove any that have gone off screen.

; For testing
(define mI (make-sprite (make-posn 300 0) 
                        (make-posn 0 5) 
                        missile-r 
                        missile-color))
(define lomI (list mI))
(define bI (make-sprite (make-posn 250 500) 
                        (make-posn -5 -1) 
                        bullet-r 
                        bullet-color))
(define bII (make-sprite (make-posn -1 300) 
                         (make-posn -3 -4) 
                         bullet-r 
                         bullet-color))
(define lobI (list bI bII))
(define worldI (make-world lomI lobI 100))

(check-expect (move-bullets worldI) (make-world 
                                     lomI 
                                     (list (make-sprite (make-posn 245 499) 
                                                        (make-posn -5 -1) 
                                                        bullet-r 
                                                        bullet-color)) 
                                     100))

(define (move-bullets world)
  (cond [(empty? (world-bullets world)) empty]
        [(on-screen? (first (world-bullets world))) 
         (make-world (world-missiles world)
                     (cons (make-sprite (make-posn (+ (posn-x (sprite-vel (first (world-bullets world)))) 
                                                      (posn-x (sprite-loc (first (world-bullets world)))))
                                                   (+ (posn-y (sprite-vel (first (world-bullets world))))
                                                      (posn-y (sprite-loc (first (world-bullets world))))))
                                        (sprite-vel (first (world-bullets world)))
                                        (sprite-size (first (world-bullets world)))
                                        (sprite-color (first (world-bullets world))))
                           (move-bullets (make-world (world-missiles world)
                                                     (rest (world-bullets world))
                                                     (world-health world))))
                     (world-health world))]
        [else (move-bullets (make-world (world-missiles world)
                                        (rest (world-bullets world))
                                        (world-health world)))]))


;-------------------------------------------------------------------------------
;; move-missiles : World -> World
;; Move the missiles one time step.

(define m! (make-sprite (make-posn 50 0) (make-posn 0 10) missile-r missile-color))
(define m@ (make-sprite (make-posn 100 0) (make-posn 0 5) missile-r missile-color))
(define lom! (list m! m@))
(define b! (make-sprite (make-posn 50 50) (make-posn 10 10) bullet-r bullet-color))
(define lob! (list b!))
(define world! (make-world lom! lob! 100))

;(check-expect (move-missiles world!) (make-world (list (make-sprite (make-posn 50 10) 
;                                                                    (make-posn 0 10) 
;                                                                    missile-r 
;                                                                    missile-color)
;                                                       (make-sprite (make-posn 100 5) 
;                                                                    (make-posn 0 5) 
;                                                                    missile-r 
;                                                                    missile-color))
;                                                 lob!
;                                                 100))
#;
(define (move-missiles world)
  (cond [(empty? (world-missiles world)) empty]
        [else (make-world (list (make-sprite (make-posn (+ (posn-x (sprite-vel (first (world-missiles world))))
                                                           (posn-x (sprite-loc (first (world-missiles world)))))
                                                        (+ (posn-y (sprite-vel (first (world-missiles world))))
                                                           (posn-y (sprite-loc (first (world-missiles world))))))
                                             (sprite-vel (first (world-missiles world)))
                                             (sprite-size (first (world-missiles world)))
                                             (sprite-color (first (world-missiles world))))
                                (move-missiles (make-world (rest (world-missiles world))
                                                           (world-bullets world)
                                                           (world-health world))))
                          (world-bullets world)
                          (world-health world))]))


;-------------------------------------------------------------------------------
;; remove-dead-missiles: World -> World
;; Remove every missile that is colliding with some bullet.

; For testing
(define ma (make-sprite (make-posn 50 50) (make-posn 1 1) missile-r missile-color))
(define mb (make-sprite (make-posn 100 100) (make-posn 1 1) missile-r missile-color))
(define mc (make-sprite (make-posn 150 150) (make-posn 1 1) missile-r missile-color))
(define ba (make-sprite (make-posn 51 51) (make-posn 1 1) bullet-r bullet-color))
(define bb (make-sprite (make-posn 151 151) (make-posn 1 1) bullet-r bullet-color))
(define bc (make-sprite (make-posn 10 10) (make-posn 1 1) bullet-r bullet-color))
(define lom1 (list ma mb mc))
(define lob1 (list ba bb bc))
(define world1 (make-world lom1 lob1 100))


;(check-expect (remove-dead-missiles world1) (make-world (list mc) lob1 100))
#;
(define (remove-dead-missiles world)
  (cond [(empty? (world-bullets world)) (make-world
                                         (world-missiles world)
                                         (world-bullets world)
                                         (world-health world))]
        [(collision? (first lom) (first lob)) (make-world
                                               (world-missiles (list (rest world)))
                                               (world-bullets world)
                                               (world-health world))]))


;-------------------------------------------------------------------------------
;; detonate-missiles: World -> World
;; Remove missiles that landed... and decrement the player's health if any did.

; For testing
(define mz (make-sprite (make-posn 50 500) (make-posn 0 10) missile-r missile-color))
(define mx (make-sprite (make-posn 50 250) (make-posn 0 10) missile-r missile-color))
(define lomz (list mz mx))
(define bz (make-sprite (make-posn 20 20) (make-posn 1 1) bullet-r bullet-color))
(define lobz (list bz))
(define worldz (make-world lomz lobz 100))

(check-expect (detonate-missiles worldz) (make-world (list mx) (list bz) 99))

(define (detonate-missiles world)
  (cond [(empty? (world-missiles world)) empty]
        [(<= world-height (posn-y (sprite-loc (first (world-missiles world))))) (detonate-missiles (make-world (rest (world-missiles world))
                                                                                                               (world-bullets world)
                                                                                                               (- (world-health world) missile-damage)))]
        
        [else (make-world (cons (first (world-missiles world)) (detonate-missiles (make-world (rest (world-missiles world))
                                                                                              (world-bullets world)
                                                                                              (world-health world))))
                          (world-bullets world)
                          (world-health world))]))


;-------------------------------------------------------------------------------
;; maybe-fire-missle: World -> World
;; If we haven't maxed out on missiles, launch another one.

(define los% (list mz mz mz mz mz mz mz mz mz mz))
(define los%% (list mz mz mz))
(define world% (make-world los% lobz 100))
(define world%% (make-world los%% lobz 100))

(check-expect (maybe-fire-missile world%) world%)

(define (maybe-fire-missile world)
  (cond [(< (list-length (world-missiles world)) max-missiles) 
         (make-world (append (list (make-sprite (make-posn (random 500) 0)
                                                (make-posn 0 (+ 1 (random 9)))
                                                missile-r
                                                missile-color))
                             (world-missiles world))
                     (world-bullets world)
                     (world-health world))]
        [else (make-world (world-missiles world)
                          (world-bullets world)
                          (world-health world))]))


;-------------------------------------------------------------------------------
; ON-TICK HANDLER
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;; update-world: World -> World
;; Step the world one tick.

