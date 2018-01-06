#lang racket

(require redex "spec.rkt")

(provide win? valid?)

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