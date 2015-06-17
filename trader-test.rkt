#lang racket

(require rackunit
         "trader.rkt")

(define my-ship (create-ship "Enterprise" 5 10000))

(check-equal? (location-distance (location 0 0) (location 3 4)) 5)

(check-equal? (location-distance (location 12 5) (location 0 0)) 13)

(test-begin
 (let ([loc1 (location 25 25)]
       [loc2 (location 50 50)])
   (check-equal? (location-angle loc1 loc2) (atan 1))))

(test-begin
 (let* ([player (player "Name" 1000 my-ship (location 25 25) (location 25 25))]
        [universe (universe player (list terminus trantor))])
   (check-equal? (player-location (universe-player (step universe))) (location 25 25))))

(test-begin
 (let* ([player (player "Name" 1000 my-ship (location 25 25) (location 26 26))]
        [universe (universe player (list terminus trantor))])
   (check-equal? (player-location (universe-player (step universe))) (location 26 26))))

(test-begin
 (let* ([player (player "Name" 1000 my-ship (location 25 25) (location 25 80))]
        [universe (universe player (list terminus trantor))])
   (check-equal? (player-location (universe-player (step universe))) (location 25 30))))

(test-begin
 (let* ([player (player "Name" 1000 my-ship (location 25 25) (location 80 25))]
        [universe (universe player (list terminus trantor))])
   (check-equal? (player-location (universe-player (step universe))) (location 30 25))))

(test-begin
 (let* ([player (player "Name" 1000 my-ship (location 25 25) (location 25 2))]
        [universe (universe player (list terminus trantor))])
   (check-equal? (player-location (universe-player (step universe))) (location 25 20))))

(test-begin
 (let* ([player (player "Name" 1000 my-ship (location 25 25) (location 2 25))]
        [universe (universe player (list terminus trantor))])
   (check-equal? (player-location (universe-player (step universe))) (location 20 25))))

(test-begin
 (let* ([ship (create-ship "test ship" 13 10000)]
        [player (player "Name" 1000 ship (location 10 24) (location 0 0))]
        [universe (universe player (list terminus trantor))])
   (check-equal? (player-location (universe-player (step universe))) (location 5 12))))

(test-begin
 (let* ([ship (create-ship "test ship" 13 10000)]
        [player (player "Name" 1000 ship (location 24 10) (location 0 0))]
        [universe (universe player (list terminus trantor))])
   (check-equal? (player-location (universe-player (step universe))) (location 12 5))))

(test-begin
 (let* ([ship (create-ship "test ship" 13 10000)]
        [player (player "Name" 1000 ship (location 10 10) (location 10 10))]
        [universe (universe player (list terminus trantor))])
   (check-equal? (at-destination? universe) true)))

(test-begin
 (let* ([ship (create-ship "test ship" 13 10000)]
        [player (player "Name" 1000 ship (location 10 10) (location 23 42))]
        [universe (universe player (list terminus trantor))])
   (check-equal? (at-destination? universe) false)))

(test-begin
 (let* ([ship (create-ship "test ship" 13 10000)]
        [player (player "Name" 1000 ship (location 10 10) (location 23 42))]
        [universe (universe player (list terminus trantor))]
        [updated-universe (star-map-loop universe)])
   (check-equal? (player-location player) (location 23 42))))
