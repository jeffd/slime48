;;; -*- mode: scheme48; package: swank-repl -*-

;;;;;; SLIME for Scheme48
;;;;;; Read-eval-print loop operations

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (repl-eval-string string)
  (let ((exp (read-from-string string)))
    (if (eof-object? exp)
        (values)
        (receive results (eval exp (interaction-environment))
          (force-output (current-output-port))
          (apply values results)))))

(define (repl-eval-string* string zero one many)
  (receive results (repl-eval-string string)
    (cond ((null?      results)  (zero))
          ((null? (cdr results)) (one (car results)))
          (else                  (many results)))))

; (put 'repl-eval-string* 'scheme-indent-function 1)

(define (swank:interactive-eval string)
  (receive results (repl-eval-string string)
    (interactive-eval-results results)))

(define (interactive-eval-results results)
  (cond ((null? results) "; No value")
        ((null? (cdr results))
         (let ((v (car results)))
           (if (integer? v)
               (string-append (number->string v 10)
                              " (#x" (number->string v 16)
                              ", #o" (number->string v 8)
                              ", #b" (number->string v 2)
                              ")")
               (write-to-string v))))
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
                  (else "; No value"))
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
        (receive vals (repl-eval-string string)
          (list (string-output-port-output port)
                (delimited-object-list-string vals write newline)))))))

(define (swank:pprint-eval string)
  (repl-eval-string* string
    (lambda () "; No value")
    (lambda (v) (pp-to-string v))
    (lambda (vals)
      (delimited-object-list-string vals p ""))))

(define (swank:listener-eval string)
  (repl-eval-string* string
    (lambda ()
      (record-swank-repl-result '())
      "; No value")
    (lambda (v)
      (record-swank-repl-result v)
      (write-to-string v))
    (lambda (vals)
      (record-swank-repl-result vals)
      (delimited-object-list-string vals write newline))))

(define-swank-session-slot record-swank-repl-results?
  set-record-swank-repl-results?!
  modify-record-swank-repl-results?!
  #t)

(define (enable-swank-repl-recording)
  (set-record-swank-repl-results?! #t))
(define (disable-swank-repl-recording)
  (set-record-swank-repl-results?! #f))
(define (toggle-swank-repl-recording)
  (modify-record-swank-repl-results?! not))

(define-swank-session-slot swank-repl-results
  set-swank-repl-results!
  modify-swank-repl-results!
  '())

(define (record-swank-repl-result result)
  (cond ((and (record-swank-repl-results?)
              (current-swank-return-tag))
         => (lambda (return-tag)
              (modify-swank-repl-results!
               (lambda (results)
                 (cons (cons return-tag result) results)))))))

(define (swank:get-repl-result return-tag)
  (cond ((not (record-swank-repl-results?))
         (error "Swank REPL results not being recorded"
                `(GET-REPL-RESULT ,return-tag)))
        ((assv return-tag (swank-repl-results))
         => cdr)
        (else
         (error "Swank REPL result no longer exists"
                `(GET-REPL-RESULT ,return-tag)))))

(define (swank:clear-last-repl-result)
  (modify-swank-repl-results! cdr)
  't)

(define (swank:clear-repl-results)
  (set-swank-repl-results! '())
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
                  (list id (write-to-string
                            (or name
                                (package-uid package)))))))
          (else (abort-swank-rpc)))))
