#lang racket

(provide (all-defined-out))

; name: String
; money: Number
; ship: Ship
; location: Location
; destination: Location
(struct player (name money ship location destination) #:mutable #:transparent)

; name: String
; speed: positive Number
; inventory: Hash (Product . Natural Number)
; capacity: Natural Number
(struct ship (name speed inventory capacity))

; name: String
; volume: positive Number
; base-price: positive Number
(struct product (name volume base-price))

; name: String
; location: Location
; inventory: Hash (Product . Natural Number)
; product-prices: Hash (Product . Function (Product, Inventory))
(struct planet (name location inventory product-prices))

; x: Natural Number
; y: Natural Number
(struct location (x y) #:transparent)

; player: Player
; planets: [Planet]
; products: [Product]
(struct universe (player planets))

;;; Locations

; Returns the angle (in radians) between loc and destination
; loc: Location
; destination: Location
(define (location-angle loc destination)
  (let ([dx (- (location-x destination) (location-x loc))]
        [dy (- (location-y destination) (location-y loc))])
    (cond
     [(and (= dx 0) (= dy 0)) #f]
     [(= dx 0) (* (/ pi 2) (/ dy (abs dy)))]
     [else (atan (/ dy dx))])))

; Returns the distance between two locations
; loc: Location
; destination: Location
(define (location-distance loc destination)
  (sqrt (+ (expt (- (location-x destination) (location-x loc)) 2)
           (expt (- (location-y destination) (location-y loc)) 2))))

; Returns true if the player is currently at the destination, false otherwise
; universe: Universe
(define (at-destination? universe)
  (let ([player (universe-player universe)])
    (equal? (player-location player) (player-destination player))))
  


;;; Products
(define uranium (product "uranium" 1 100))
(define all-products (list uranium))

;;; Ships
(define (create-ship name speed capacity)
  (let ([inventory (make-hash (map (lambda (product) (cons product 0)) all-products))])
    (ship name speed inventory capacity)))

;;; Planets

; Function for generating very simple price-generation functions. They take a base price
; and an inventory, and return a price equal to (base price * multiplier)
(define (simple-multiplier-generator multiplier)
  (lambda (product inventory)
    (* (product-base-price product) multiplier)))

(define terminus
  (planet
   "terminus"
   (location 80 80)
   (make-hash (list (cons uranium 100)))
   (make-hash (list (cons uranium (simple-multiplier-generator .6))))))

(define trantor
  (planet
   "trantor"
   (location 800 500)
   (make-hash (list (cons uranium 50)))
   (make-hash (list (cons uranium (simple-multiplier-generator 1.2))))))

;;; Game State
(define (step universe)
  (let* ([player (universe-player universe)]
         [destination (player-destination player)]
         [loc (player-location player)])
    (if (equal? loc destination)
        universe
        (let ([new-loc (step-player-location loc destination (ship-speed (player-ship player)))])
          (set-player-location! player new-loc)
          universe))))

; loc: Location
; destination: Location
; speed: positive Number
(define (step-player-location loc destination speed)
  (let ([distance (location-distance loc destination)])
    (if (< distance speed)
        destination
        (let* ([theta (location-angle loc destination)]
               ; I can't be bothered to actually thing about the trig, so we just
               ; take the absolute value of dy and dx and manually figure out the sign
               [dx (abs (inexact->exact (round (* (cos theta) speed))))]
               [dy (abs (inexact->exact (round (* (sin theta) speed))))]
               [x-sign (if (> (location-x destination) (location-x loc)) + -)]
               [y-sign (if (> (location-y destination) (location-y loc)) + -)])
          (location
           (x-sign (location-x loc) dx)
           (y-sign (location-y loc) dy))))))
          

  
  
