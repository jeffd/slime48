;;; -*- mode: scheme; scheme48-package: limited-writing -*-

;;;;;; SLIME for Scheme48
;;;;;; Limited writing utilities

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (limited-write obj port)
  (s48-limited-write obj port (write-depth) (write-length)))

(define $write-depth (make-fluid (make-cell 3)))
(define $write-length (make-fluid (make-cell 5)))

(define (write-depth) (fluid-cell-ref $write-depth))
(define (write-length) (fluid-cell-ref $write-length))

(define (set-write-depth! d) (fluid-cell-set! $write-depth d))
(define (set-write-length! l) (fluid-cell-set! $write-length l))

(define (with-limited-output depth length thunk)
  (let-fluids $write-depth (make-cell depth)
              $write-length (make-cell length)
    thunk))

(define (saving-output-limits thunk)
  (with-limited-output (fluid-cell-ref $write-depth)
                       (fluid-cell-ref $write-length)
    thunk))
