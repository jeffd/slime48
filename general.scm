;;; -*- Mode: Scheme; scheme48-package: swank-general-rpc -*-

;;;; SLIME for Scheme48
;;;; Implementations of General, Miscellaneous RPC Interfaces

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (swank:connection-info)
  `(:STYLE :SPAWN
    :LISP-IMPLEMENTATION (:TYPE "Scheme 48"
                          :NAME "s48"
                          ;; Argh!  Bad hard-coding!  Bad!
                          :VERSION "1.3")
    :MACHINE (:INSTANCE ,(get-host-name))
    :FEATURES ()
    :PACKAGE (:NAME "(user)"
              :PROMPT "(user)")
    ))

(define $swank-quitter
  (make-fluid terminate-current-swank-session))

(define (with-swank-quitter quitter thunk)
  (let-fluid $swank-quitter quitter thunk))

(define (swank:quit-lisp)
  ((fluid $swank-quitter)))

(define (swank:default-directory)
  (working-directory))

(define (swank:set-default-directory dir)
  (set-working-directory! dir)
  dir)
