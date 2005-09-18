;;; -*- mode: scheme48; package: swank-worlds -*-

;;;;;; SLIME for Scheme48
;;;;;; Swank worlds: session collections & environment state

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;++ What else belongs here?  This module is pretty empty...

(define-record-type* swank-world
  (make-swank-world scratch-env         ; user scratch environment
                    config-env          ; module language environment
                    rpc-env             ; Swank RPC binding environment
                    id)
  ((sessions (make-weak-set))))

(define-record-discloser :swank-world
  (lambda (world)
    (list 'swank-world (swank-world-id world))))

(define (swank-world-structure-names world)
  (config-structure-names (swank-world-config-env world)))

(define (swank-world-package-names world)
  (append '((scratch) (config) (swank-rpc))
          (config-package-names (swank-world-config-env world))))

(define (find-structure-in-swank-world id world)
  (config-find-structure (swank-world-config-env world) id))

(define (find-package-in-swank-world id world)
  (cond ((and (pair? id)
              (symbol? (car id))
              (null? (cdr id)))
         (case (car id)
           ((scratch)   (swank-world-scratch-env world))
           ((config)    (swank-world-config-env  world))
           ;; Is access to the RPC environment useful?
           ((swank-rpc) (swank-world-rpc-env     world))
           (else #f)))
        (else
         (config-find-package (swank-world-config-env world) id))))



;;; ----------------
;;; Environment initialization

(define (make-swank-envs scheme config-lang built-in rpcs)
  (let* ((tower (make-reflective-tower eval (list scheme) 'scheme))
         ;; The second argument to MAKE-SIMPLE-PACKAGE is #T to specify
         ;; that the package is unstable, i.e. code can be loaded into
         ;; it dynamically; it doesn't have statically affixed code.
         (scratch-env (make-simple-package (list scheme) #t tower
                                           '(scratch)))
         (config-env (make-config-package config-lang built-in tower
                                          '(config)))
         (rpc-env (make-simple-package
                   (list (make-modified-structure scheme
                                                  ;; Nothing but QUOTE
                                                  ;; from R5RS Scheme.
                                                  '((expose quote)))
                         rpcs)
                   #t
                   (delay (error
                      "macro definitions are disallowed in Swank RPC"))
                   '(swank-rpc))))

    ;; Turn off integration of primitives so that the user can redefine
    ;; them.
    (set-package-integrate?! scratch-env #f)

    ;; Artifacts from Maclisp (because the original SLIME is built for
    ;; elisp & Common Lisp).  I don't know whether these are really
    ;; necessary, but it's too much effort to figure out whether they
    ;; are or aren't.
    ;++ Much more important: is NIL punned as false & ()?
    (environment-define! rpc-env 't #t)
    (environment-define! rpc-env 'nil #f)

    (values scratch-env config-env rpc-env)))
