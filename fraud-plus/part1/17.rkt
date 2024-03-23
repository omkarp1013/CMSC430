#lang racket

(let* ((x #t) (y (not x))) (boolean? y))