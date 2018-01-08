#lang racket

(require redex "spec.rkt")

(provide win? valid? isomorphic?)

; consumes either
; - a board and produces #t iff some player has won on that board
; - a board and a player term and produces #t iff the specified
; player has won on that board
(define win?
  (case-lambda
    [(board) (or (redex-match? tic-tac-toe (any_1
                                            ...
                                            [player_1 player_1 player_1]
                                            any_2
                                            ...)
                               board)
                 (redex-match? tic-tac-toe ([any_1 ..._1 player_1 any_2 ..._2]
                                            [any_3 ..._1 player_1 any_4 ..._2]
                                            [any_5 ..._1 player_1 any_6 ..._2])
                               board)
                 (redex-match? tic-tac-toe ([player_1 any_1 any_2]
                                            [any_3 player_1 any_4]
                                            [any_5 any_6 player_1])
                               board)
                 (redex-match? tic-tac-toe ([any_1 any_2 player_1]
                                            [any_3 player_1 any_4]
                                            [player_1 any_5 any_6])
                               board))]
    [(board player) (or (redex-match? tic-tac-toe
                                      (side-condition (any_1
                                                       ...
                                                       [(name winner player_1) player_1 player_1]
                                                       any_2
                                                       ...)
                                                      (equal? (term winner) player))
                                      board)
                        (redex-match? tic-tac-toe
                                      (side-condition ([any_1 ..._1 (name winner player_1) any_2 ..._2]
                                                       [any_3 ..._1 player_1 any_4 ..._2]
                                                       [any_5 ..._1 player_1 any_6 ..._2])
                                                      (equal? (term winner) player))
                                      board)
                        (redex-match? tic-tac-toe
                                      (side-condition ([(name winner player_1) any_1 any_2]
                                                       [any_3 player_1 any_4]
                                                       [any_5 any_6 player_1])
                                                      (equal? (term winner) player))
                                      board)
                        (redex-match? tic-tac-toe
                                      (side-condition ([any_1 any_2 (name winner player_1)]
                                                       [any_3 player_1 any_4]
                                                       [player_1 any_5 any_6])
                                                      (equal? (term winner) player))
                                      board))]))

; consumes a board and produces #t iff the number
; of Xs on the board is exactly equal to the number
; of Os on the board, or exactly one greater
(define (valid? board)
  (let ([x-count (flat-count (term X) board)]
        [o-count (flat-count (term O) board)])
    (or (= x-count o-count) (= x-count (+ o-count 1)))))

; consumes a 3x3 board and produces a 3x3 board which is
; rotated once counter-clockwise
(define rotate
  (reduction-relation tic-tac-toe
                      (--> ([position_1 position_2 position_3]
                            [position_4 position_5 position_6]
                            [position_7 position_8 position_9])
                           ([position_7 position_4 position_1]
                            [position_8 position_5 position_2]
                            [position_9 position_6 position_3]))))

; consumes a board and produces a list of all unique rotations of the board.
; including the initial board.
; note that this depends on redex's implementation of -->*, which has a few
; interesting side-effects
; note also that this depends on the rotate reduction relation, so is
; guaranteed to produce non-empty lists only on 3x3 boards
; 1. boards with fewer than 4 unique rotations (e.g. the empty/inital board)
; will produce a list less than length 4. Again, these are *unique* rotations
; 2. because this relies on redex's cycle detection to terminate the proc,
; the initial-board appears last in the list rather than first
(define (rotations board)
  (apply-reduction-relation* rotate
                             board
                             #:all? #t))

; consumes two 3x3 boards and returns #t iff the one board is equal to
; or a rotation of the other
; note that this is only guaranteed to work on 3x3 boards
(define (isomorphic? board1 board2)
  (ormap ((curry equal?) board1) (rotations board2)))

                         