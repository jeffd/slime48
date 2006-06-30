;;; -*- Mode: Scheme; scheme48-package: extended-writing -*-

;;;; Writer Extensions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;++ Other options: pretty-printing, radix, base, readable, fill...

;++ Using RECURRING-WRITE is probably a loss in the end; the built-in
;++ writer is just too limited.

(define $write-depth (make-fluid (make-cell 3)))
(define $write-breadth (make-fluid (make-cell 5)))

(define (write-depth) (fluid-cell-ref $write-depth))
(define (write-breadth) (fluid-cell-ref $write-breadth))
(define (set-write-depth! d) (fluid-cell-set! $write-depth d))
(define (set-write-breadth! b) (fluid-cell-set! $write-breadth b))

(define (with-writing-limits depth breadth thunk)
  (let-fluids $write-depth (make-cell depth)
              $write-breadth (make-cell breadth)
    thunk))

;++ I really don't like the name `circular'; it has nothing to do with
;++ circularity, only shared references.  But CL set precedent with
;++ *PRINT-CIRCLE*, and I accidentally called the procedure
;++ CIRCULAR-WRITE a while ago.  Foo.

(define $write-limited? (make-fluid (make-cell #t)))
(define $write-circular? (make-fluid (make-cell #t)))

(define (write-limited?) (fluid-cell-ref $write-limited?))
(define (write-circular?) (fluid-cell-ref $write-circular?))
(define (set-write-limited?! t) (fluid-cell-set! $write-limited? t))
(define (set-write-circular?! s) (fluid-cell-set! $write-circular? s))

(define (limited-write obj port)
  (with-limited-writing (lambda () (extended-write obj port))))

(define (circular-write obj port)
  (with-circular-writing (lambda () (extended-write obj port))))

(define (hybrid-write obj port)
  (with-hybrid-writing (lambda () (extended-write obj port))))

(define (with-limited-writing thunk)
  (let-fluids $write-limited? (make-cell #t)
              $write-circular? (make-cell #f)
    thunk))

(define (with-circular-writing thunk)
  (let-fluids $write-limited? (make-cell #f)
              $write-circular? (make-cell #t)
    thunk))

(define (with-hybrid-writing thunk)
  (let-fluids $write-limited? (make-cell #t)
              $write-circular? (make-cell #t)
    thunk))

(define (with-preserved-writing thunk)
  (let-fluids $write-depth (fluid-cell-ref $write-depth)
              $write-breadth (fluid-cell-ref $write-breadth)
              $write-limited? (fluid-cell-ref $write-limited?)
              $write-circular? (fluid-cell-ref $write-circular?)
    thunk))

(define (write-atomic? datum)
  (or (number? datum)
      (string? datum)
      (boolean? datum)
      (char? datum)
      (symbol? datum)
      (eof-object? datum)
      (null? datum)))

(define (extended-write obj port)
  (let ((max-depth (and (write-limited?) (write-depth)))
        (max-breadth (and (write-limited?) (write-breadth)))
        (circular? (write-circular?)))
    (let ((shared-data
           (and circular?
                (compute-shared-data obj max-depth max-breadth))))

      (define (write-shared obj entry depth)
        (write-char #\# port)
        (write-string (number->string (shared-entry-number entry) 10)
                      port)
        (cond ((not (shared-entry-written? entry))
               (shared-entry-written! entry)
               (write-char #\= port)
               ((if (pair? obj) write-pair recur) obj depth))
              (else
               (write-char #\# port))))

      (define (write-pair pair depth)
        (write-char #\( port)
        (let ((depth* (+ depth 1)))
          (write-dispatch (car pair) depth*)
          (let loop ((tail (cdr pair)) (breadth 1))
            (cond ((write-atomic? obj)
                   (write-string " . " port)
                   (recur obj #f))
                  ((shared-entry shared-data tail)
                   => (lambda (entry)
                        (write-string " . " port)
                        (write-shared tail entry depth*)))
                  ((and max-breadth (>= breadth max-breadth))
                   (write-string " ---" port))
                  ((pair? tail)
                   (write-char #\space port)
                   (write-dispatch (car tail) depth*)
                   (loop (cdr tail) (+ breadth 1)))
                  ((not (null? tail))
                   (write-string " . " port)
                   (write-dispatch tail depth*)))))
        (write-char #\) port))

      (define (write-dispatch obj depth)
        (cond ((write-atomic? obj)
               (recur obj #f))
              ((and max-depth (>= depth max-depth))
               (write-string "#" port))
              ((shared-entry shared-data obj)
               => (lambda (entry)
                    (write-shared obj entry depth)))
              ((pair? obj)
               (write-pair obj depth))
              (else
               (recur obj depth))))

      (define (recur obj depth)
        (recurring-write obj port
          (lambda (obj)
            (if depth
                (write-dispatch obj (+ depth 1))
                (error "non-atomic object thought to be atomic"
                       obj
                       depth)))))

      (write-dispatch obj 0))))

(define (make-shared-entry datum number) (cons datum (cons number #f)))
(define (shared-entry-datum entry) (car entry))
(define (shared-entry-number entry) (car (cdr entry)))
(define (shared-entry-written? entry) (cdr (cdr entry)))
(define (shared-entry-written! entry) (set-cdr! (cdr entry) #t))
(define (shared-entry shared-data obj)
  (and shared-data (assq obj shared-data)))

;++ Making this use hash tables would be easy and a good idea, if we
;++ had general object tables, which Scheme48 does not (yet).

(define (compute-shared-data datum max-depth max-breadth)
  (let ((port (make-null-output-port))
        (shared-data '())
        (seen-data '())
        (number 0))

    (define (share datum)
      (set! shared-data
            (cons (make-shared-entry datum number) shared-data))
      (set! number (+ number 1)))

    (define (shared? datum) (assq datum shared-data))

    (define (see datum)
      (set! seen-data (cons (cons datum '()) seen-data)))

    (define (seen? datum) (assq datum seen-data))

    (define (walk-list list depth breadth)
      (if (pair? list)
          (if (not (shared? list))
              (if (seen? list)
                  (share list)
                  (begin (see list)
                         (walk (car list) depth)
                         (let ((tail (cdr list)))
                           (if (not (and max-breadth
                                         (>= breadth max-breadth)))
                               (walk-list tail
                                          depth
                                          (+ breadth 1)))))))
          (walk list depth)))

    (define (walk datum depth)
      (if (not (or (and max-depth
                        (>= depth max-depth))
                   (write-atomic? datum)))
          (if (seen? datum)
              (share datum)
              (begin (see datum)
                     (let ((depth* (+ depth 1)))
                       (if (pair? datum)
                           (begin (walk (car datum) depth*)
                                  (walk-list (cdr datum) depth* 1))
                           (recurring-write datum port
                             (lambda (datum*)
                               (walk datum* depth*)))))))))

    (walk datum 0)

    shared-data))
