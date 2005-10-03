;;; -*- mode: scheme; scheme48-package: swank-repl -*-

;;;;;; SLIME for Scheme48
;;;;;; Read-eval-print loop operations

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (repl-eval-string string)
  (let ((exp (read-from-string string)))
    (if (eof-object? exp)
        #f
        (receive results (eval exp (interaction-environment))
          (force-output (current-output-port))
          results))))

(define (repl-eval-string* string nothing zero one many)
  (let ((results (repl-eval-string string)))
    (cond ((not        results)  (nothing))
          ((null?      results)  (zero))
          ((null? (cdr results)) (one (car results)))
          (else                  (many results)))))

; (put 'repl-eval-string* 'scheme-indent-function 1)

(define (swank:interactive-eval string)
  (interactive-eval-results (repl-eval-string string)))

(define (interactive-eval-results results)
  (cond ((not   results) "; Nothing to evaluate")
        ((null? results) "; No value")
        ((null? (cdr results))
         (let ((v (car results)))
           (if (integer? v)
               (string-append (number->string v 10)
                              " (#x" (number->string v 16)
                              ", #o" (number->string v 8)
                              ", #b" (number->string v 2)
                              ")")
               (circular-write-to-string v))))
        (else
         (delimited-object-list-string results
                                       interactive-limited-write
                                       ", "))))

(define (swank:interactive-eval-region string)
  (let ((port (make-string-input-port string)))
    (let loop ((results #f))
      (let ((exp (read port)))
        (if (eof-object? exp)
            (cond (results => interactive-eval-results)
                  (else "; Nothing to evaluate"))
            (receive results (eval exp (interaction-environment))
              (loop results)))))))

(define (interactive-limited-write obj port)
  (limited-write obj port (repl-print-depth) (repl-print-length)))
(define (repl-print-depth) 3)
(define (repl-print-length) 4)

(define (swank:eval-and-grab-output string)
  (let ((port (make-string-output-port)))
    (call-with-current-output-port port
      (lambda ()
        (cond ((repl-eval-string string)
               => (lambda (vals)
                    (list (string-output-port-output port)
                          (delimited-object-list-string vals write
                                                        newline))))
              (else (list "" "; Nothing to evaluate")))))))

(define (swank:pprint-eval string)
  (repl-eval-string* string
    (lambda () "; Nothing to evaluate")
    (lambda () "; No value")
    (lambda (v) (pp-to-string v))
    (lambda (vals)
      (delimited-object-list-string vals p ""))))

(define (swank:listener-eval string)
  (cond ((repl-eval-string string)
         => (lambda (results)
              (if (swank-repl-presentations?)
                  (repl-present results)
                  `(:VALUES ,(map circular-write-to-string results)))))
        (else '(:SUPPRESS-OUTPUT))))

(define (repl-present results)
  (let ((table (swank-repl-presentations)))
    (reduce ((list* result results))
        ((id (swank-repl-presentation-id))
         (presentations '()))
      (begin (weak-table-set! table id (canonicalize-false result))
             (values (+ id 1)
                     `((,(circular-write-to-string result)
                        . ,id)
                       ,@presentations)))
      (begin (set-swank-repl-presentation-id! id)
             `(:PRESENT ,(reverse presentations))))))

(define-swank-session-slot swank-repl-presentation-id
  set-swank-repl-presentation-id!
  modify-swank-repl-presentation-id!
  0)

(define-swank-session-slot swank-repl-presentations
  set-swank-repl-presentations!
  modify-swank-repl-presentations!
  #f)

(define-swank-session-slot swank-repl-presentations?
  set-swank-repl-presentations?!
  modify-swank-repl-presentations?!
  #f)

(define (enable-swank-repl-presentations)
  (set-swank-repl-presentations?! #t)
  (set-swank-repl-presentations! (make-weak-table)))
(define (disable-swank-repl-presentations)
  (set-swank-repl-presentations?! #f)
  (set-swank-repl-presentations! #f))
(define (toggle-swank-repl-presentations)
  (modify-swank-repl-presentations?!
   (lambda (x)
     (cond (x
            (set-swank-repl-presentations! #f)
            #f)
           (else
            (set-swank-repl-presentations! (make-weak-table))
            #t)))))

(define (swank:get-repl-result id)
  (cond ((not (swank-repl-presentations?))
         (error "Swank REPL presentations disabled"
                `(GET-REPL-RESULT ,id)))
        ((weak-table-ref (swank-repl-presentations) id)
         => decanonicalize-false)
        (else
         (error "Swank REPL presentation no longer exists"
                `(GET-REPL-RESULT ,id)))))

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
  (set-swank-repl-presentations! (if (swank-repl-presentations?)
                                     (make-weak-table)
                                     #f))
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
                (set-interaction-environment! package)
                (let ((name (package-name package)))
                  (if (not (or (equal? id name)
                               (eq? (find-package-in-swank-world name
                                                                 world)
                                    package)))
                      (warn "package identification mismatch"
                            id package))
                  (list id (circular-write-to-string
                            (or name
                                (package-uid package)))))))
          (else (abort-swank-rpc)))))

(define (swank:use-package string)
  (let ((id (read-from-string string))
        (world (current-swank-world)))
    (cond ((find-structure-in-swank-world id world)
           (package-open! (interaction-environment)
                          (lambda ()
                            ;; Don't eagerly cache the value, because
                            ;; it may change during development.
                            (find-structure-in-swank-world id world)))
           'nil)
          (else (abort-swank-rpc)))))
