;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Question4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Assignment#2 Question #4
;Ankush Varshneya
;100853074
;Game Description
;-use left and right error to move the shooter
;-press space to fire a bullet to shoot balls before it falls of the screen
;-if you shoot all balls before they fall to the ground then you win
;-balls change color and update the number on then to show
; the score value you can gain by shooting them
;-ever time a ball (or a group of balls that have the same y-position)
; fall off the screen you loose a life if you loose all lives you loose the game
;-shooting a ball(or group of balls with x positions really close) 
; updates the list and makes the ball(s) disappear of the screen

(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 600)
(define HEIGHT 600)

(define NOT-FIRED 0)
(define FIRED 1)
(define IN-AIR 2)

(define LEFT 1)
(define RIGHT 2)

(define SHOOTER-HEIGHT 40)
(define SHOOTER (triangle SHOOTER-HEIGHT "solid" "brown"))
(define SHOOTER-SPEED 10)

(define BULLET (circle 5 "solid" "white"))
(define BULLET-SPEED 100)

(define NUM-BALL 10)
(define BALL-START-Y 50)

(define MAX-BALL-SCORE 600)

(define MAX-LIVES 3)

; the CANVAS: black background
(define CANVAS (place-image
                (rectangle WIDTH HEIGHT "solid" "Black")
                (/ WIDTH 2)
                (/ HEIGHT 2)
                (empty-scene WIDTH HEIGHT)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Data structure to represent the changing parts of the world.
; It contains:
; - the shooter's position
; - has a bullet been fired
; - the bullet's position
; - list of balls
; - grand score
; - lives
(define-struct world (shooter-posn bullet-state bullet-posn ball-list score lives))

; The initial state of the world when the program starts
; Data structure to represent a planet.
; It contains:
; - the ball's radius
; - the ball's speed
; - the ball's score
; - the ball's posn
; - the ball's color
(define-struct ball (radius speed score posn color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
; Helper functions


;draw the score and lives on the scene/CANVAS
(define (place-score-live current-world scene)
  (place-image
   (text (string-append "Lives:" (number->string (world-lives current-world))) 20 "white")
   (- WIDTH 35)
   15
   (place-image
    (text (string-append "Score:" (number->string (world-score current-world))) 20 "white")
    60
    15
    scene)))

;find if no lives left
(define (no-lives? current-world) (<= (world-lives current-world) 0))

;finds if no balls remaining
(define (no-balls? current-world) (null? (world-ball-list current-world)))

;finds if a life was lost in this frame my checking if any balls fell of the screen
(define (life-lost? ball-list bullet-posn)
  (not
   (null?
    (filter
     (lambda (ball)
       (not (< (+ (posn-y (ball-posn ball)) (ball-radius ball)) HEIGHT)))
     ball-list))))

;finds score by getting list of balls were hit by a bullet and adding up their scores
(define (get-score ball-list bullet-posn)
  (letrec 
      ([add-up-score
        (lambda (sub-list)
          (if (null? sub-list)
              0
              (+ (ball-score (car sub-list))
                 (add-up-score (cdr sub-list)))))])
    (add-up-score
     (filter
      (lambda (ball)
        (not
         (or
          (< (posn-x bullet-posn) (- (posn-x (ball-posn ball)) (ball-radius ball)))
          (> (posn-x bullet-posn) (+ (posn-x (ball-posn ball)) (ball-radius ball))))))
      ball-list))))

;place all the balls
(define (place-ball sub-list scene)
  (if (null? sub-list)
      scene
      (place-image
       (place-image
        (text (number->string (ball-score (car sub-list))) 15 "white")
        (ball-radius (car sub-list))
        (ball-radius (car sub-list))
        (circle (ball-radius (car sub-list)) "solid" (ball-color (car sub-list))))
       (posn-x (ball-posn (car sub-list)))
       (posn-y (ball-posn (car sub-list)))
       (place-ball (cdr sub-list) scene))))

;initilize the balls
(define (init-balls num-ball)
  (if (zero? num-ball)
      empty
      (cons
       (make-ball 20 (+ (random 4) 1) MAX-BALL-SCORE (make-posn (* 50 num-ball) BALL-START-Y) "blue")
       (init-balls (sub1 num-ball)))))

;update the balls list will balls that are still alive
(define (update-balls ball-list bullet-posn)
  (map
   (lambda (ball)
     (make-ball
      (ball-radius ball)
      (ball-speed ball)
      (- (ball-score ball) (ball-speed ball))
      (make-posn (posn-x (ball-posn ball)) (+ (posn-y (ball-posn ball)) (ball-speed ball)))
      (update-ball-color (ball-score ball))))
   ;only balls that do not collide and are still have posn-y >0 with the bullet are allowed
   (filter
    (lambda (ball)
      (and
       (or
        (< (posn-x bullet-posn) (- (posn-x (ball-posn ball)) (ball-radius ball)))
        (> (posn-x bullet-posn) (+ (posn-x (ball-posn ball)) (ball-radius ball))))
       (< (+ (posn-y (ball-posn ball)) (ball-radius ball)) HEIGHT)))
    ball-list)))

;update ball color based on score
(define (update-ball-color score)
  (cond [(> score 500) "green"]
        [(> score 400) "greenyellow"]
        [(> score 300) "yellow"]
        [(> score 200) "orange"]
        [(> score 100) "orangered"]
        [else "red"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
; Functions to pass to big-bang

; The initial state of the world when the program starts
(define INIT-WORLD (make-world
                    (make-posn (/ WIDTH 2) (- HEIGHT (* SHOOTER-HEIGHT (cos (/ pi 3)))))
                    NOT-FIRED
                    (make-posn 0 0)
                    (init-balls NUM-BALL)
                    0 ;score is originally zero
                    MAX-LIVES))

; Take the current world state given to us by the universe and draw the appropriate scene
(define (redraw current-world)
  (place-score-live
   current-world
   (cond [(no-lives? current-world)
          (place-image
           (text "You Lost!" 40 "white")
           (/ WIDTH 2)
           (/ HEIGHT 2)
           CANVAS)]
         [(no-balls? current-world)
          (place-image
           (text "You Won!" 40 "white")
           (/ WIDTH 2)
           (/ HEIGHT 2)
           CANVAS)]
         [else
          (place-ball
           (world-ball-list current-world)
           (if (= (world-bullet-state current-world) NOT-FIRED)
               (place-image SHOOTER (posn-x (world-shooter-posn current-world)) (posn-y (world-shooter-posn current-world)) CANVAS)
               (place-image BULLET (posn-x (world-bullet-posn current-world)) (posn-y (world-bullet-posn current-world))
                            (place-image SHOOTER (posn-x (world-shooter-posn current-world)) (posn-y (world-shooter-posn current-world)) CANVAS))))])))

; Take the current world and make a new one using an updated shooter position and a bullet as needed
(define (change-current-world-key current-world a-key-event)
  (cond
    [(key=? a-key-event "left")
     (make-world
      (make-posn (modulo(- (posn-x (world-shooter-posn current-world)) SHOOTER-SPEED) WIDTH) (posn-y (world-shooter-posn current-world)))
      (world-bullet-state current-world)
      (world-bullet-posn current-world)
      (world-ball-list current-world)
      (world-score current-world)
      (world-lives current-world))]
    [(key=? a-key-event "right")
     (make-world
      (make-posn (modulo(+ (posn-x (world-shooter-posn current-world)) SHOOTER-SPEED) WIDTH) (posn-y (world-shooter-posn current-world)))
      (world-bullet-state current-world)
      (world-bullet-posn current-world)
      (world-ball-list current-world)
      (world-score current-world)
      (world-lives current-world))]
    [(key=? a-key-event " ")
     (make-world
      (world-shooter-posn current-world)
      FIRED
      (world-shooter-posn current-world)
      (world-ball-list current-world)
      (world-score current-world)
      (world-lives current-world))]
    [else ;return current-world on invalid keypress
     current-world]))

;updates the bullet/balls movements
(define (change-current-world-tick current-world)
  (make-world
   (world-shooter-posn current-world)
   ;bullet-state
   (if (= (world-bullet-state current-world) FIRED)
       IN-AIR
       (world-bullet-state current-world))
   ;bullet-posn
   (if (= (world-bullet-state current-world) NOT-FIRED)
       (world-bullet-posn current-world)
       (make-posn (posn-x (world-bullet-posn current-world)) (- (posn-y (world-bullet-posn current-world)) BULLET-SPEED)))
   (update-balls (world-ball-list current-world) (world-bullet-posn current-world))
   (+ (world-score current-world) (get-score (world-ball-list current-world) (world-bullet-posn current-world)))
   (if (life-lost? (world-ball-list current-world) (world-bullet-posn current-world))
       (sub1 (world-lives current-world))
       (world-lives current-world))))

; Ask whether game is over
(define (game-over? current-world) (or (no-lives? current-world) (no-balls? current-world)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(big-bang INIT-WORLD ; the "state" in its initial form - it will be passed around to the other functions below
          (on-draw redraw) ; draw the scene again with current "state"
          (on-tick change-current-world-tick) ; update "state"
          (on-key change-current-world-key) ; update the scene according to key presses
          (stop-when game-over?)) ; check whether everything should stop (win, lose?)