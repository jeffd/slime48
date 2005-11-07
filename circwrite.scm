;;; -*- mode: scheme; scheme48-package: circular-writing -*-

;;;;;; SLIME for Scheme48
;;;;;; Circular writer utility (CL-style #n=... & #n#)

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (circular-write obj port)
  (let ((alist (compute-shared-structure obj)))

    (define (write-shared probe)
      (write-char #\# port)
      (write-string (number->string (cddr probe) 10)
                    port)
      (cond ((cadr probe)
             (set-car! (cdr probe) #f)
             (write-char #\= port)
             (let ((obj (car probe)))
               (if (pair? obj)
                   (write-list obj)
                   (recurring-write obj port recur))))
            (else
             (write-char #\# port))))

    (define (write-list obj)
      (write-char #\( port)
      (recur (car obj))
      (let loop ((obj (cdr obj)))
        (cond ((assq obj alist)
               => (lambda (probe)
                    (write-string " . " port)
                    (write-shared probe)))
              ((pair? obj)
               (write-char #\space port)
               (recur (car obj))
               (loop (cdr obj)))
              ((not (null? obj))
               (write-string " . " port)
               (recur obj))))
      (write-char #\) port))

    (define (recur obj)
      (cond ((assq obj alist)
             => write-shared)
            ((pair? obj)
             (write-list obj))
            (else
             (recurring-write obj port recur))))

    (recur obj)))

(define (compute-shared-structure obj)
  (let ((port (make-null-output-port))
        (shared '())
        (seen '())
        (count 0))

    (define (interesting? obj)
      (not (or (number? obj)            ; Order of presentation here is
               (string? obj)            ; totally random.
               (boolean? obj)
               (char? obj)
               (symbol? obj)
               (eof-object? obj)
               (null? obj)
               (assq obj shared))))

    (define (share obj)
      (set! shared (cons (cons obj (cons #t count))
                         shared))
      (set! count (+ count 1)))

    (define (see obj)
      (set! seen (cons (cons obj '()) seen)))

    (define (recur-list list)
      (cond ((pair? list)
             (if (not (assq list shared))
                 (if (assq list seen)
                     (share list)
                     (begin (see list)
                            (recur (car list))
                            (recur-list (cdr list))))))
            ((interesting? list)
             (if (assq list seen)
                 (share list)
                 (begin (see list)
                        (recurring-write list port recur))))))

    (define (recur obj)
      (if (interesting? obj)
          (cond ((assq obj seen) (share obj))
                (else
                 (see obj)
                 (if (pair? obj)
                     (begin (recur (car obj))
                            (recur-list (cdr obj)))
                     (recurring-write obj port recur))))))

    (recur obj)
    shared))