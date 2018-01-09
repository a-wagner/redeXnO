#lang racket

(require redex "../src/strategy.rkt" "../src/pred.rkt")

(test-equal (andmap (lambda (board) (and (valid? board) (win? board)))
                      winning-boards)
              #t)

(test-equal (andmap (lambda (board) (and (valid? board) (win? board)))
                      unique-winning-boards)
              #t)

(test-results)