#lang racket

(require racket/gui/base
         racket/draw
         "trader.rkt")

(define PLANET-DIAMETER 20)
(define STEP-MILLIS 500)

#|
(define planet-panel (new horizontal-panel%
                          [parent frame]
                          [min-width 1200]
                          [min-height 900]))
|#



(define (draw-planet planet dc)
  (let ([loc (planet-location planet)]) 
    (send dc draw-ellipse
          (- (location-x loc) (/ PLANET-DIAMETER 2))
          (- (location-y loc) (/ PLANET-DIAMETER 2))
          PLANET-DIAMETER
          PLANET-DIAMETER)
    (let-values ([(w _ __ ___) (send dc get-text-extent (planet-name planet))])
      (send dc draw-text
            (planet-name planet)
            (- (location-x loc) (/ w 2))
            (+ (location-y loc) PLANET-DIAMETER)))))

(define (draw-player player dc)
  (let* ([loc (player-location player)]
        [rocket (read-bitmap "rocket.png")]
        [rocket-width (send rocket get-width)]
        [rocket-height (send rocket get-height)]
        [rocket-angle (location-angle (player-location player) (player-destination player))]
        [rocket-dc (send rocket make-dc)])
;    (display rocket-angle)
;    (newline)
;    (send dc set-rotation rocket-angle)
    (send dc draw-bitmap
          rocket
          (- (location-x loc) (/ rocket-width 2))
          (- (location-y loc) (/ rocket-height 2)))))

(define (at-planet? universe)
  (let ([player (universe-player universe)])
    (letrec ([loop (lambda (planets)
                     (if (null? planets)
                         #f
                         (let* ([planet (car planets)]
                                [planet-x (location-x (planet-location planet))]
                                [planet-y (location-y (planet-location planet))]
                                [player-x (location-x (player-location player))]
                                [player-y (location-y (player-location player))])
                           (if (and (<= planet-x player-x)
                                    (<= player-x (+ planet-x PLANET-DIAMETER))
                                    (<= planet-y player-y)
                                    (<= player-y (+ planet-y PLANET-DIAMETER)))
                               planet
                               (loop (cdr planets))))))])
      (loop (universe-planets universe)))))

(define (star-map-step universe)
  (step universe))

(define star-timer%
  (class timer%
    (init-field universe)
    (init-field frame)
    (init-field transition)
;    (define universe universe)
;    (define frame frame)
    (super-new)
    (inherit stop)
    (define/override (notify)
      (if (at-destination? universe)
          (begin
            (stop)
            (let ([cur-planet (at-planet? universe)])
              (when cur-planet
                (display cur-planet)
                (newline)
                (transition cur-planet)
                (send frame refresh))))
          (begin
            (star-map-step universe)
            (send frame refresh))))))

(define star-canvas%
  (class canvas%
    (init-field timer)
    (init-field universe)
    (super-new)
    (define/override (on-paint)
      (let ([dc (send this get-dc)])
        (for-each (lambda (planet) (draw-planet planet dc)) (universe-planets universe))
        (draw-player (universe-player universe) dc)))
    (define/override (on-event event)
      (let ([event-type (send event get-event-type)]
            [x (send event get-x)] 
            [y (send event get-y)])
        (when (equal? event-type 'left-down)
          (set-player-destination! (universe-player universe) (location x y))
          (send timer start STEP-MILLIS))))))

(define nav-panel%
  (class vertical-panel%
    (init-field timer)
    (init-field universe)
    (super-new [alignment (list 'center 'top)]
               [stretchable-width #f])
    (new message%
     [label "Set Destination"]
     [parent this])
    (for-each (lambda (planet)
            (new button%
                 [label (planet-name planet)]
                 [parent this]
                 [callback (lambda (b e)
                             (send timer start STEP-MILLIS)
                             (set-player-destination! (universe-player universe) (planet-location planet)))]))
          (universe-planets universe))))
    

; Creates returns a panel for navigation between planets
; frame: Frame
(define (create-star-panel frame the-universe transition-fn)
  (let ([star-panel (new horizontal-panel%
                         [parent frame]
                         [min-width 1200]
                         [min-height 900])])
    (let ([star-timer (new star-timer%
                        [universe the-universe]
                        [frame frame]
                        [transition (lambda (planet) (transition-fn star-panel planet))])])
    ; Star Canvas
      (new star-canvas%
           [universe the-universe]
           [timer star-timer]
           [parent star-panel]
           [min-width 900])
    ; Nav Panel
      (new nav-panel%
           [parent star-panel]
           [min-width 300]
           [timer star-timer]
           [universe the-universe]))))

(define (create-planet-panel frame planet universe transition-fn)
  ;(send frame delete-child nav-panel)
  (let ([planet-panel (new horizontal-panel%
                           [parent frame]
                           [min-width 1200]
                           [min-height 900])])
;;  (send frame add-child planet-panel)
  (new message%
     [label "I'm the planet panel!"]
     [parent planet-panel])
  (new button%
       [label "Leave planet"]
       [parent planet-panel]
       [callback (lambda (b e)
                   (transition-fn planet-panel))])))

(define (main)
  (define frame (new frame%
                   [label "Merchant Prince"]
                   [width 1200]
                   [height 900]))
  (define the-universe (universe (player "Alex" 1000 (create-ship "Enterprise" 40 10000) (location 25 25) (location 25 25))
                                 (list terminus trantor)))
  (letrec ([planet-view (lambda (old-child planet)
                          (send frame delete-child old-child)
                          (create-planet-panel frame planet the-universe star-view))]
           [star-view (lambda (old-child)
                        (send frame delete-child old-child)
                        (create-star-panel frame the-universe planet-view))])
    (let ([star-panel (create-star-panel frame the-universe planet-view)])
      (send frame show #t))))

(main)
