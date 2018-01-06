#lang racket

(require redex "spec.rkt" "pred.rkt")

(provide winning-boards)

(define winning-boards
  (apply-reduction-relation* move
                             (term initial-board)
                             #:cache-all? #t
                             #:stop-when (lambda (board) (win? board))))