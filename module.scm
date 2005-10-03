;;; -*- mode: scheme; scheme48-package: module-control -*-

;;;;;; Programmatic module system control utilities

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (make-config-package config-lang built-in tower name)
  (let ((config (make-simple-package (list config-lang built-in)
                                     #t      ; unstable
                                     tower
                                     name)))
    ;; Needed before any uses of DEFINE-STRUCTURE in the environment.
    (set-reflective-tower-maker!
     config
     (lambda (clauses id)
       (if (null? clauses)
           tower
           (delay (let ((package (eval `(A-PACKAGE ((FOR-SYNTAX ,id))
                                                   ,@clauses)
                                       config)))
                    (silently (lambda ()
                                (load-package package)))
                    (cons eval package))))))
    config))

(define (config-find-structure env id)
  (and-let* (((name? id))
             (binding (package-lookup env id))
             ((binding? binding))
             ((not (eq? (binding-type binding) syntax-type)))
             (loc (binding-place binding))
             ((location? loc))
             ((location-assigned? loc))
             (value (contents loc))
             ((structure? value)))
    value))

(define (config-find-package env id)
  (cond ((config-find-structure env id)
         => (lambda (struct)
              (let ((package (structure-package struct)))
                (and (package-unstable? package)
                     package))))
        (else #f)))

(define (fold-config-structures env init combiner)
  (let* ((state init)
         (combine (lambda (name binding)
                    (and-let* (((binding? binding))
                               (loc (binding-place binding))
                               ((location-assigned? loc))
                               (value (contents loc))
                               ((structure? value)))
                      (set! state (combiner name value state))))))
    (for-each-definition combine env)
    (for-each (lambda (struct)
                (for-each-export (lambda (name type binding)
                                   (combine name binding))
                                 struct))
              (package-opens env))
    state))

; (put 'fold-config-structures 'scheme-indent-function 2)

(define (config-structure-names env)
  (fold-config-structures env '()
    (lambda (name struct tail)
      (cons name tail))))

(define (config-package-names env)
  (fold-config-structures env '()
    (lambda (name struct tail)
      (if (package-unstable? (structure-package struct))
          (cons name tail)
          tail))))

(define (maybe-structure-ref struct id succeed fail)
  ;; The #F argument to STRUCTURE-LOOKUP tells it not to bother leaving
  ;; integration information in; it doesn't really matter here.
  (maybe-binding-value (structure-lookup struct id #f)
                       id struct
                       succeed fail))

(define (maybe-environment-ref env id succeed fail)
  (maybe-binding-value (package-lookup env id)
                       id env
                       succeed fail))

; (put 'maybe-structure-ref 'scheme-indent-function 2)
; (put 'maybe-environment-ref 'scheme-indent-function 2)

(define (maybe-binding-value binding id env succeed fail)
  (if (binding? binding)
      (if (not (eq? (binding-type binding)
                    syntax-type))
          (let ((loc (binding-place binding)))
            (if (and (location? loc)
                     (location-defined? loc))
                (succeed (contents loc))
                (fail (make-vm-exception
                       (enum op global)
                       (enum exception undefined-global)
                       loc))))
          (fail (make-condition 'error
                                `("name bound to syntax"
                                  ,id ,env))))
      (fail (make-condition 'error
                            `("invalid variable reference"
                              ,id ,env)))))

(define (package-reflective-tower package)
  (environment-macro-eval (package->environment package)))
