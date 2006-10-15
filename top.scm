;;; -*- mode: scheme; scheme48-package: slime48 -*-

;;;;;; SLIME for Scheme48
;;;;;; Primary entry points

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (serve-one-slime48-session session . port)
  (receive (port-number server closer) (make-slime48-tcp-server port)
    (swank-log "(session ~S) Serving one remote session on port ~S"
               (swank-session-id session)
               port-number)
    (server slime48-authenticator
            (lambda (connector)
              (connect-swank-session session connector)
              (closer))
            (lambda ()
              (swank-log "(session ~S) Client failed authentication"
                         (swank-session-id session))))))

(define (spawn-slime48-tcp-server world . port)
  (receive (port-number server closer) (make-slime48-tcp-server port)
    (let ((server-thread
           (spawn (lambda ()
                    (swank-log "(world ~S) Serving on port ~S"
                               (swank-world-id world)
                               port-number)
                    (serve-slime48-sessions server world))
                  `(slime48-server ,(swank-world-id world)))))
      (lambda ()
        (terminate-thread! server-thread)
        (closer)))))

(define (serve-slime48-sessions server world)
  (server slime48-authenticator
          (lambda (connector)
            (connect-swank-session (spawn-slime48-session world)
                                   connector))
          values)
  (serve-slime48-sessions server world))

(define (make-slime48-tcp-server port-opt)
  (cond ((null? port-opt) (make-swank-tcp-server))
        ((or (and (string? (car port-opt))   ;name of file to write
                  (lambda (port-number)
                    (call-with-output-file (car port-opt)
                      (lambda (output-port)
                        (write port-number output-port)))))
             (and (procedure? (car port-opt))
                  (car port-opt)))
         => (lambda (announcer)
              (receive (port-number server closer)
                       (make-swank-tcp-server)
                (announcer port-number)
                (values port-number server closer))))
        ((integer? (car port-opt))      ;port number
         (make-swank-tcp-server (car port-opt)))
        (else
         (error "invalid port number option" port-opt))))

(define slime48-authenticator
  (lambda (in out)
    (let ((secret-key (slime48-secret-key)))
      (or (not secret-key)
          (equal? secret-key (decode-swank-message in))))))

(define (make-slime48-world)
  (make-swank-world (make-swank-scratch-package (list scheme)
                                                (list scheme))
                    ;; Use the existing config package, which has all
                    ;; structures in Scheme48's image, not just the
                    ;; statically linked ones plus the Swank ones.
                    (config-package)
                    (make-swank-rpc-package scheme swank-rpc)
                    'SLIME48))

(define (spawn-slime48-session world . debugger-attacher)
  (call-with-result-placeholder
    (lambda (session-placeholder)
      (with-sldb-handler #f
          (lambda ()
            (placeholder-value session-placeholder))
          (and (pair? debugger-attacher)
               (wrap-debugger-attacher (car debugger-attacher)))
        (lambda ()
          (with-slime48-port-redirection session-placeholder
            (lambda ()
              (spawn-swank-session world values values))))))))

(define (wrap-debugger-attacher procedure)
  (let ((spawn (spawner-on-current-scheduler)))
    (lambda (condition session)
      (let ((placeholder (make-placeholder)))
        (spawn (lambda ()
                 (placeholder-set! placeholder
                                   (procedure condition session)))
               `(swank-teledebugger ,(swank-session-id session)))
        (placeholder-value placeholder)))))

(define (with-slime48-port-redirection session-placeholder body)
  ;++ What about the noise and error output ports?
  (call-with-current-output-port
      (let ((out (make-swank-output-port session-placeholder)))
        (periodically-force-output! out)
        out)
    (lambda ()
      (call-with-current-input-port
          (make-swank-input-port session-placeholder)
        body))))



;++ Is the placeholder the right abstraction here?  What about promises
;++ or thunks built on top of placeholders?

(define (call-with-result-placeholder procedure)
  (let* ((placeholder (make-placeholder))
         (value (procedure placeholder)))
    (placeholder-set! placeholder value)
    value))

(define (spawner-on-current-scheduler)
  (let ((scheduler (thread-scheduler (current-thread))))
    (lambda (thunk . name)
      (apply spawn-on-scheduler scheduler thunk name))))

(define (read-line input-port)
  (let loop ((chars '())
             (count 0))
    (let ((char (read-char input-port)))
      (cond ((eof-object? char)
             (if (zero? count)
                 char
                 (reverse-list->string chars count)))
            ((char=? char #\newline)
             (reverse-list->string chars count))
            (else
             (loop (cons char chars) (+ count 1)))))))



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
                           ;++ This is wrong!
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
  (string-upcase (hybrid-write-to-string (location-name loc))))

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
