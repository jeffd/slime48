;;; -*- mode: scheme; scheme48-package: swank-worlds -*-

;;;;;; SLIME for Scheme48
;;;;;; Swank worlds: session collections & environment state

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;++ What else belongs here?  This module is pretty empty...

(define-record-type* swank-world
  (make-swank-world user-env            ; user scratch environment
                    config-env          ; module language environment
                    rpc-env             ; Swank RPC binding environment
                    id)
  ((sessions (make-weak-table))
   (session-id (make-cell 0))))

(define-record-discloser :swank-world
  (lambda (world)
    (list 'swank-world (swank-world-id world))))

(define (next-swank-session-id world)
  (let ((cell (swank-world-session-id world)))
    (call-ensuring-atomicity
      (lambda ()
        (let ((id (provisional-cell-ref cell)))
          (provisional-cell-set! cell (+ id 1))
          id)))))

(define (swank-world-structure-names world)
  (config-structure-names (swank-world-config-env world)))

(define (swank-world-package-names world)
  (append '((user) (config) (swank-rpc))
          (config-package-names (swank-world-config-env world))))

(define (find-structure-in-swank-world id world)
  (config-find-structure (swank-world-config-env world) id))

(define (find-package-in-swank-world id world)
  (cond ((and (pair? id)
              (symbol? (car id))
              (null? (cdr id)))
         (case (car id)
           ((user)      (swank-world-user-env    world))
           ((config)    (swank-world-config-env  world))
           ;; Is access to the RPC environment useful?
           ((swank-rpc) (swank-world-rpc-env     world))
           (else #f)))
        (else
         (config-find-package (swank-world-config-env world) id))))



;;; ----------------
;;; Environment initialization

(define (make-swank-user-package opens tower-opens)
  (make-simple-package opens
                       #t               ;unstable
                       (make-reflective-tower eval tower-opens 'SCHEME)
                       '(USER)))

(define (make-swank-config-package config-lang built-in tower-opens)
  (make-config-package config-lang
                       built-in
                       (make-reflective-tower eval tower-opens 'SCHEME)
                       '(CONFIG)))

(define (make-swank-rpc-package scheme rpc)
  (let ((rpc-env
         (make-simple-package
          (list (make-modified-structure scheme
                                         ;; Nothing but QUOTE from
                                         ;; R5RS Scheme.
                                         '((expose quote)))
                rpc)
          #t                            ;unstable
          (delay (error
                  "macro definitions are disallowed in Swank RPC"))
          '(SWANK-RPC))))
    ;; Artifacts from Maclisp (because the original SLIME is built for
    ;; elisp & Common Lisp).  I don't know whether these are really
    ;; necessary, but it's too much effort to figure out whether they
    ;; are or aren't.
    ;++ Much more important: is NIL punned as false & ()?
    (environment-define! rpc-env 'T #t)
    (environment-define! rpc-env 'NIL #f)
    rpc-env))
