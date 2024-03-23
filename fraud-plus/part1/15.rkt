#lang racket

(let* ((x 1) (y x) (z (add1 y))) (+ x y z))