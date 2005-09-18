;;; -*- mode: scheme; scheme48-package: continuation-data-type -*-

;;;;;; Continuation data type, more general than CWCC escape procedures

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type continuation :continuation
  (make-continuation procedure)
  continuation?
  (procedure continuation-procedure))

(define (reify-continuation receiver)
  ((call-with-current-continuation
     (lambda (procedure)
       (lambda ()
         (receiver (make-continuation procedure)))))))

(define (throw-to-continuation continuation thunk)
  ((continuation-procedure continuation) thunk))

(define (return-from-continuation continuation . returned-values)
  (throw-to-continuation continuation
                         (lambda ()
                           (apply values returned-values))))

(define (with-continuation continuation thunk)
  (call-with-values thunk
    (lambda returned-values
      (apply return-from-continuation continuation returned-values))))

; (put 'reify-continuation 'scheme-indent-function 0)
; (put 'throw-to-continuation 'scheme-indent-function 1)
; (put 'with-continuation 'scheme-indent-function 1)
