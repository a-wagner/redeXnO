#lang racket

(require redex "spec.rkt" "pred.rkt")

(provide winning-boards unique-winning-boards)

; a list of all boards in which one of X or O wins
(define winning-boards
  (apply-reduction-relation* move
                             (term initial-board)
                             #:cache-all? #t
                             #:stop-when (lambda (board) (win? board))))

; a list of *representative* boards in which one of X or O wins.
; representative here means that each board in the list represents
; the class of boards which are rotations of itself
(define unique-winning-boards
  (map first (group-by identity winning-boards isomorphic?)))