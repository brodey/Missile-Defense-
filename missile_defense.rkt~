;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname missile_defense) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)

;; A SPRITE is a (make-sprite Posn Posn Number Symbol) where:
;; - LOC is the sprite's location
;; - VEL is its velocity
;; - SIZE is the radius of the sprite
;; - COLOR is its color.
;; Location, velocity and size are in computer-graphic/pixel coordinates.
;; A sprite represents either an attacker's missile or a defender's
;; anti-missile bullet.
(define-struct sprite (loc vel size color))

;; A LOS (list of sprites) is one of:
;; - empty
;; - (cons SPRITE LOS)

;; A world structure is a (make-world LOS LOS Number) where:
;; - MISSILES: the missiles attacking the player
;; - BULLETS: the missiles launched by the player
;; - HEALTH: current health of the player -- game-over if health <= 0
(define-struct world (missiles bullets health))

; Constants
(define world-width 500)
(define world-height 500)
(define gun-position 250)
(define missile-vel (make-posn 0 5))
(define missile-r 10)
(define missile-color 'red)
(define missile (circle missile-r 'solid missile-color))
(define bullet-r 5)
(define bullet-color 'green)
(define bullet (circle bullet-r 'solid bullet-color))


; For testing
(define m1 (make-sprite (make-posn 100 100) (make-posn -3 2) missile-r missile-color))
(define m2 (make-sprite (make-posn 50 50) (make-posn 1 1) missile-r missile-color))
(define m3 (make-sprite (make-posn 50 500) (make-posn 1 0) missile-r missile-color))
(define b1 (make-sprite (make-posn -50 50) (make-posn -5 5) bullet-r bullet-color))
(define b2 (make-sprite (make-posn 45 50) (make-posn 2 3) bullet-r bullet-color))


;; on-screen? : SPRITE -> Boolean
;; Given a sprite, produces a boolean--true if the sprite is on screen,
;; false if it is not.

(check-expect (on-screen? m1) true)
(check-expect (on-screen? b1) false)
 
(define (on-screen? sprite)
  (cond [(and (and (< 0 (posn-x (sprite-loc sprite))) 
                   (< (posn-x (sprite-loc sprite)) world-width))
              (and (< 0 (posn-y (sprite-loc sprite)))
                   (< (posn-x (sprite-loc sprite)) world-height))) true]
        [else false]))

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

;; missile-impact? : SPRITE -> Boolean
;; Given a sprite, produces a boolean--true if the sprite has reached
;; the bottom of the world, false if it has not

(check-expect (missile-impact? m1) false)
(check-expect (missile-impact? m3) true)

(define (missile-impact? sprite)
  (cond [(<= world-height (posn-y (sprite-loc sprite))) true]
        [else false]))

;; 


                   

;; move-bullets : World -> World
;; Move the bullets one time step and remove any that have gone off screen.






;; move-missiles : World -> World
;; Move the missiles one time step.

;; remove-dead-missiles: World -> World
;; Remove every missile that is touching some bullet.

;; detonate-missiles: World -> World
;; Remove missiles that landed... and decrement the player's health if any did.

;; maybe-fire-missle: World -> World
;; If we haven't maxed out on missiles, launch another one.

;; update-world: World -> World
;; Step the world one tick.



