#lang racket

(+ 1 (if (begin (write-byte 1) 1) (begin (write-byte 1) 1) (begin (write-byte 2) 2)))