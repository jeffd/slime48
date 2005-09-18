;;; -*- mode: scheme48; package: slime48 -*-

;;;;;; SLIME for Scheme48
;;;;;; Primary entry points

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (slime48 . port-number)
  (let ((world (make-slime48-world)))
    (values world (apply spawn-slime48-tcp-server world port-number))))

(define (make-slime48-world)
  (receive (scratch config rpc)
           (make-swank-envs scheme module-system
                            built-in-structures
                            swank-rpc)
    (make-swank-world scratch
                      ;; Use the existing config package, which has all
                      ;; structures in Scheme48's image, not just the
                      ;; statically linked ones plus the Swank ones.
                      (config-package)
                      rpc
                      'slime48)))

(define (spawn-slime48-tcp-server world . port-number)
  (apply spawn-swank-tcp-server world
         (lambda (session-placeholder body)
           (with-sldb-handler #f
             (lambda ()
               (with-slime48-port-redirection session-placeholder
                 body))))
         port-number))

(define (with-slime48-port-redirection session-placeholder body)
  (call-with-current-output-port
      (make-swank-output-port session-placeholder)
    (lambda ()
      (call-with-current-input-port
          (make-swank-input-port session-placeholder)
        (lambda ()
          ;; No init or exit thunks.  (Hook for separate TCP stream.)
          (body values values))))))



;;; Handling undefined global errors

;;; This is copied roughly from env/shadow.scm; this code is copyright
;;; (C) 1993-2005 by Richard Kelsey and Jonathan Rees.  It is licensed
;;; under the modified BSD licence.  This is necessary because we need
;;; access to the template & index in order to support USE-VARIABLE
;;; restarters, but env/shadow.scm discards them when signalling VM
;;; exceptions.  (I do not know why.)

(define (replaced-variables-handler with-restarters win)
  (lambda (opcode reason loc template index . rest)
    (let ((lose (lambda ()
                  (with-restarters loc template index
                    (lambda ()
                      (apply signal-vm-exception opcode reason loc
                             template index
                             rest))))))
      (if (= reason (enum exception undefined-global))
          (deal-with-replaced-variable opcode reason loc template index
                                       rest win lose)
          (lose)))))

(define (deal-with-replaced-variable opcode reason loc template index
                                     rest win lose)
  (if (eq? (template-ref template index) loc)
      (let* ((p-uid (template-package-id template))
             (new (maybe-replace-location loc p-uid)))
        (if (eq? new loc)
            (lose)
            (begin (template-set! template index new)
                   (if (location-defined? new)
                       (win new rest)
                       (lose)))))
      (error "lossage in DEAL-WITH-REPLACED-VARIABLE"
             opcode reason loc template index rest)))

(define maybe-replace-location
  (let ((memv memv))                    ; Huh?
    (lambda (loc p-uid)
      (let ((foo (location-id loc)))
        (if (vector? foo)
            (maybe-replace-location
             (if (memv p-uid (vector-ref foo 1))
                 (vector-ref foo 2)
                 (vector-ref foo 0))
             p-uid)
            loc)))))

(define (with-undefined-global-restarters loc template index body)
  (let ((name (location-name-as-string loc)))
    (call-with-current-continuation
      (lambda (return)
        (call-with-interactive-restarter 'use-value
            (string-append "Specify a value to use instead of " name
                           ".")
            (lambda (val) (return val))
            (lambda () (prompt-for-evaluated-expression
                        "Value to use"))
          (lambda (use-value)
            (call-with-interactive-restarter 'store-value
                (string-append "Define " name
                               " to a given value and use it.")
                (lambda (val)
                  (set-contents! loc val)
                  (return val))
                (lambda () (prompt-for-evaluated-expression
                            "Value to store"))
              (lambda (store-value)
                (with-variable-replacement-restarter
                    loc template index
                  body)))))))))

;++ This is slightly wrong: it should be more careful about stable &
;++ unstable packages, local & non-local references, &c.

(define (with-variable-replacement-restarter loc template index body)
  (let ((name (location-name-as-string loc)))
    (call-with-interactive-restarter 'use-variable
        (string-append "Specify a different variable to use"
                       " in the place of " name ".")
        (lambda (name package)
          (cond ((and-let* ((binding (package-lookup package name))
                            (loc (binding-place binding))
                            ((location? loc)))
                   (template-set! template index loc)
                   loc)
                 => contents)
                (else
                 (error "invalid replacement variable"
                        name package))))
        (lambda ()
          (let ((package (structure-package
                          (prompt-for-evaluated-expression
                           "Package of structure"
                           (config-package)))))
            (values (prompt-for-name "Name of variable")
                    package)))
      (lambda (use-variable)
        (body)))))

(define-vm-exception-handler (enum op global)
  (replaced-variables-handler
   with-undefined-global-restarters
   (lambda (loc args)
     (contents loc))))

(define-vm-exception-handler (enum op set-global!)
  (replaced-variables-handler
   with-variable-replacement-restarter
   (lambda (loc args)
     (set-contents! loc (car args)))))

(define (location-name-as-string loc)
  (string-upcase (write-to-string (location-name loc))))

(define (string-upcase string)
  (let* ((len (string-length string))
         (result (make-string len)))
    (do ((i 0 (+ i 1)))
        ((= i len) result)
      (string-set! result i
                   (char-upcase (string-ref string i))))))



;++ Implement these better some day.  Swank should provide interaction-
;++ side calls to prompt the user for things.

(define (prompt-for-name prompt)
  (let ((out (current-output-port))
        (in (current-input-port)))
    (let loop ()
      (display prompt out)
      (display " (not evaluated): " out)
      (force-output out)
      (let ((name (ignore-errors (lambda () (read in)))))
        (cond ((symbol? name)
               name)
              (else
               (display "Invalid name input." out)
               (newline out)
               (force-output out)
               (loop)))))))

(define (prompt-for-evaluated-expression prompt . env)
  ((lambda (input)
     (eval input (if (null? env)
                     (interaction-environment)
                     (car env))))
   (let ((out (current-output-port))
         (in (current-input-port)))
     (let loop ()
       (display prompt out)
       (display " (evaluated" out)
       (if (pair? env)
           (begin (display " in " out)
                  (write (car env) out)))
       (display "): " out)
       (force-output out)
       ((call-with-current-continuation
          (lambda (k)
            (lambda ()
              (with-handler (lambda (c punt)
                              (if (read-error? c)
                                  (k loop)
                                  (punt)))
                (lambda ()
                  (read in)))))))))))
