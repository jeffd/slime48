;;; -*- mode: scheme; scheme48-package: weak-utilities -*-

;;;;;; Weak utilities for Scheme48: weak sets & tail-weak hash tables
;;;;;; (thread-safe sets, thread-unsafe tables)

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type* weak-set
  (make-weak-set)
  ((lock (make-lock))
   (list (cons 'sentinel '()))))

;++ optimistic concurrency?

(define (weak-set-add! weak-set obj)
  (with-lock (weak-set-lock weak-set)
    (lambda ()
      (let ((list (weak-set-list weak-set)))
        (let loop ((list (cdr list)) (lag list))
          (cond ((null? list)
                 (set-cdr! lag (cons (make-weak-pointer obj) '())))
                ((weak-pointer-ref (car list))
                 => (lambda (elt)
                      (if (not (eq? elt obj))
                          (loop (cdr list) list))))
                (else
                 (let ((tail (cdr list)))
                   (set-cdr! lag tail)
                   (loop tail lag)))))))))

(define (weak-set-delete! weak-set obj)
  (with-lock (weak-set-lock weak-set)
    (lambda ()
      (let ((list (weak-set-list weak-set)))
        (let loop ((list (cdr list)) (lag list))
          (if (pair? list)
              (cond ((weak-pointer-ref (car list))
                     => (lambda (elt)
                          (if (eq? elt obj)
                              (set-cdr! lag (cdr list))
                              (loop (cdr list) list))))
                    (else
                     (let ((tail (cdr list)))
                       (set-cdr! lag tail)
                       (loop tail lag))))))))))

(define (weak-set-pop! weak-set)
  (with-lock (weak-set-lock weak-set)
    (lambda ()
      (let ((list (weak-set-list weak-set)))
        (let loop ((list (cdr list)) (lag list))
          (if (null? list)
              #f
              (let ((weak (car list))
                    (tail (cdr list)))
                (set-cdr! lag tail)
                (or (weak-pointer-ref weak)
                    (loop tail lag)))))))))

(define (weak-set-fold weak-set init-seed combiner)
  (weak-set-fold* weak-set init-seed
                  (lambda (obj seed)
                    (values #t (combiner obj seed)))))

(define (weak-set-fold* weak-set init-seed combiner)
  (with-lock (weak-set-lock weak-set)
    (lambda ()
      (let ((list (weak-set-list weak-set)))
        (let loop ((list (cdr list))
                   (lag list)
                   (seed init-seed))
          (cond ((null? list) seed)
                ((weak-pointer-ref (car list))
                 => (lambda (obj)
                      (receive (proceed? seed) (combiner obj seed)
                        (if proceed?
                            (loop (cdr list) list seed)
                            seed))))
                (else
                 (let ((tail (cdr list)))
                   (set-cdr! lag tail)
                   (loop tail lag seed)))))))))

; (put 'weak-set-fold 'scheme-indent-function 2)
; (put 'weak-set-fold* 'scheme-indent-function 2)

(define (weak-set-walk weak-set proc)
  (with-lock (weak-set-lock weak-set)
    (lambda ()
      (let ((list (weak-set-list weak-set)))
        (let loop ((list (cdr list)) (lag list))
          (if (pair? list)
              (cond ((weak-pointer-ref (car list))
                     => (lambda (obj)
                          (proc obj)
                          (loop (cdr list) list)))
                    (else
                     (let ((tail (cdr list)))
                       (set-cdr! lag tail)
                       (loop tail lag))))))))))

; (put 'weak-set-walk 'scheme-indent-function 1)

(define (weak-set-empty? weak-set)
  (weak-set-fold* weak-set #t (lambda (obj seed) (values #f #f))))

(define (weak-set-find weak-set pred)
  (weak-set-fold* weak-set #f
                  (lambda (obj seed)
                    (if (pred obj)
                        (values #f obj)
                        (values #t #f)))))

(define (weak-set-contains? weak-set obj)
  (weak-set-fold* weak-set #f
                  (lambda (elt seed)
                    (if (eq? elt obj)
                        (values #f #t)
                        (values #t #f)))))

(define (weak-set->list weak-set)
  (weak-set-fold weak-set '() cons))

;++ Other operations: WEAK-SET-UNFOLD WEAK-SET-MAP

;++ Naming?  WEAK-SET-operation or operation-WEAK-SET?  Follow English
;++ grammar rules or Scheme lexeme conventions?



;;; Why is this not built-in?

(define (with-lock lock thunk)
  (dynamic-wind (lambda () (obtain-lock lock))
                thunk
                (lambda () (release-lock lock))))



;;; ----------------
;;; Tail-weak hash tables

;++ (Why not just use riatables?)

(define (make-weak-table . hash)
  (apply make-table hash))

(define (weak-table-ref table key)
  (cond ((table-ref table key)
         => (lambda (weak)
              (or (weak-pointer-ref weak)
                  (begin (table-set! table key #f)
                         #f))))
        (else #f)))

(define (weak-table-set! table key value)
  (table-set! table key (and value (make-weak-pointer value))))

(define (weak-table-walk table proc)
  (table-walk (lambda (key weak)
                (cond ((weak-pointer-ref weak)
                       => (lambda (value)
                            (proc key value)))
                      (else
                       (table-set! table key #f))))
              table))

; (put 'weak-table-walk 'scheme-indent-function 1)
