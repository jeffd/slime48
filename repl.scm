;;; -*- mode: scheme; scheme48-package: swank-repl -*-

;;;;;; SLIME for Scheme48
;;;;;; Read-eval-print loop operations

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (repl-eval-string string)
  (let ((exp (read-from-string string)))
    (if (eof-object? exp)
        #f
        (receive results
                 (eval exp (semi-loaded-interaction-environment))
          ;; Force all of the expression's output before yielding a
          ;; value, which is sent by the same mechanism as printed
          ;; output.
          (force-output (current-output-port))
          results))))

(define (swank:interactive-eval string)
  (interactive-eval-results (repl-eval-string string)))

(define (interactive-eval-results results)
  (cond ((not   results) "; Nothing to evaluate")
        ((null? results) "; No value")
        ((null? (cdr results))
         (let ((v (car results)))
           (if (and (integer? v) (exact? v))
               (string-append (number->string v 10)
                              " (#x" (number->string v 16)
                              ", #o" (number->string v 8)
                              ", #b" (number->string v 2)
                              ")")
               (hybrid-write-to-string v))))
        (else
         (delimited-object-list-string results
                                       hybrid-write
                                       ", "))))

(define (swank:interactive-eval-region string)
  (let ((port (make-string-input-port string)))
    (let loop ((results #f))
      (let ((exp (read port)))
        (if (eof-object? exp)
            (cond (results => interactive-eval-results)
                  (else "; Nothing to evaluate"))
            (receive results
                     (eval exp (semi-loaded-interaction-environment))
              (loop results)))))))

(define (swank:eval-and-grab-output string)
  (let ((port (make-string-output-port)))
    (call-with-current-output-port port
      (lambda ()
        (cond ((repl-eval-string string)
               => (lambda (vals)
                    (list (string-output-port-output port)
                          (if (null? vals)
                              "; No value"
                              (delimited-object-list-string vals
                                                            hybrid-write
                                                            newline)))))
              (else (list "" "; Nothing to evaluate")))))))

(define (swank:pprint-eval string)
  (pprint-eval-results (repl-eval-string string)))

(define (pprint-eval-results results)
  (cond ((not   results) "; Nothing to evaluate")
        ((null? results) "; No value")
        ((null? (cdr results))
         (pp-to-string (car results)))
        (else
         ;; The pretty-printer emits trailing newlines, so we need no
         ;; separator of our own here.
         (delimited-object-list-string results p ""))))

(define (swank:listener-eval string)
  (cond ((repl-eval-string string)
         => listener-eval-results)
        (else
         (send-outgoing-swank-message (current-swank-session)
           '(:WRITE-STRING "; Nothing to evaluate"
                           NIL
                           :REPL-RESULT))))
  'NIL)

(define (listener-eval-results results)
  (if (null? results)
      (send-outgoing-swank-message (current-swank-session)
        '(:WRITE-STRING "; No value" NIL :REPL-RESULT))
      (for-each (let ((record (repl-result-recorder)))
                  (lambda (result)
                    (send-outgoing-swank-message (current-swank-session)
                      `(:WRITE-STRING ,(shared-write-to-string result)
                                      ,(record result)
                                      :REPL-RESULT))))
                results)))

(define (repl-result-recorder)
  (cond ((swank-repl-presentations)
         => (lambda (presentations)
              (lambda (datum)
                (let ((id (swank-repl-presentation-id)))
                  (set-swank-repl-presentation-id! (+ id 1))
                  (weak-table-set! presentations id datum)
                  id))))
        (else
         (lambda (datum)
           datum                        ;ignore
           'NIL))))

(define-swank-session-slot swank-repl-presentation-id
  set-swank-repl-presentation-id!
  modify-swank-repl-presentation-id!
  0)

(define-swank-session-slot swank-repl-presentations
  set-swank-repl-presentations!
  modify-swank-repl-presentations!
  #f)

(define (swank-repl-presentations?)
  (if (swank-repl-presentations)
      #t
      #f))

(define (enable-swank-repl-presentations)
  (set-swank-repl-presentations! (make-weak-table)))

(define (disable-swank-repl-presentations)
  (set-swank-repl-presentations! #f))

(define (toggle-swank-repl-presentations)
  (modify-swank-repl-presentations!
   (lambda (presentations)
     (if presentations
         #f
         (make-weak-table)))))

(define (swank:get-repl-result id)
  (cond ((let ((presentations (swank-repl-presentations)))
           (and presentations
                (weak-table-ref presentations id)))
         => decanonicalize-false)
        (else
         (abort-swank-rpc "No such REPL result: ~S" id))))

(define canonicalize-false)
(define decanonicalize-false)
(let ((false-token (list 'false)))
  (set! canonicalize-false
        (lambda (x)
          (or x false-token)))
  (set! decanonicalize-false
        (lambda (x)
          (if (eq? x false-token)
              #f
              x))))

(define (swank:clear-repl-results)
  (modify-swank-repl-presentations!
   (lambda (presentations)
     (if presentations
         (make-weak-table)
         #f)))
  't)

(define (delimited-object-list-string vals write delimiter)
  (let ((delimit (if (string? delimiter)
                     (lambda (p) (write-string delimiter p))
                     delimiter)))
    (call-with-string-output-port
      (lambda (port)
        (let loop ((vals vals))
          (write (car vals) port)
          (if (pair? (cdr vals))
              (begin (delimit port)
                     (loop (cdr vals)))))))))

(define (swank:set-package string)
  (let ((id (read-from-string string))
        (world (current-swank-world)))
    (cond ((find-package-in-swank-world id world)
           => (lambda (package)
                (load-package package)
                (set-interaction-environment! package)
                (let ((name (package-name package)))
                  (if (not (or (equal? id name)
                               (eq? (find-package-in-swank-world name
                                                                 world)
                                    package)))
                      (warn "package identification mismatch"
                            id package))
                  (list (hybrid-write-to-string id)
                        (hybrid-write-to-string
                         (or name
                             (package-uid package)))))))
          (else (abort-swank-rpc
                 "(world ~S, session ~S, SET-PACKAGE) ~A: ~A"
                 (swank-world-id world)
                 (swank-session-id (current-swank-session))
                 "No such unstable package by name"
                 string)))))

(define (swank:use-package string)
  (let ((id (read-from-string string))
        (world (current-swank-world)))
    (cond ((find-structure-in-swank-world id world)
           => (lambda (struct)
                (load-package (structure-package struct))
                (package-open! (interaction-environment)
                               (lambda ()
                                 ;; Don't cache the value, because
                                 ;; it may change during development.
                                 (find-structure-in-swank-world
                                  id
                                  world)))
               'nil))
          (else (abort-swank-rpc
                 "(world ~S, session ~S, USE-PACKAGE) ~A: ~A"
                 (swank-world-id world)
                 (swank-session-id (current-swank-session))
                 "No such structure by name"
                 string)))))

(define (swank:undefine-function string)
  (package-undefine! (interaction-environment)
                     (read-from-string string))
  'nil)
