;;; -*- mode: scheme48; package: xvectors -*-

;;;;;; SLIME for Scheme48
;;;;;; Expandable vectors

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;++ cheesy temporary implementation -- replace with clever resizing
;++ vector tree implementation for O(n) or O(log n) access

(define-record-type* xvector
  (make-xvector)
  ((list '())))

(define (xvector-length xvector)
  (length (xvector-list xvector)))

(define (xvector-ref xvector index)
  (list-ref (xvector-list xvector) index))

(define (xvector-maybe-push! xvector val)
  (let ((list (xvector-list xvector)))
    (if (null? list)
        (begin (set-xvector-list! xvector (cons val '()))
               0)
        (let loop ((list list) (i 0))
          (if (eq? (car list) val)
              i
              (let ((tail (cdr list))
                    (j (+ i 1)))
                (if (null? tail)
                    (begin (set-cdr! list (cons val '()))
                           j)
                    (loop tail j))))))))

(define (xvector-push! xvector val)
  (let ((list (xvector-list xvector)))
    (if (null? list)
        (begin (set-xvector-list! xvector (cons val '()))
               0)
        (let loop ((list list) (i 0))
          (let ((tail (cdr list))
                (i (+ i 1)))
            (if (null? tail)
                (begin (set-cdr! list (cons val '()))
                       i)
                (loop tail i)))))))

(define (xvector-index xvector obj)
  (let loop ((list (xvector-list xvector))
             (index 0))
    (if (eq? obj (car list))
        index
        (loop (cdr list) (+ index 1)))))
