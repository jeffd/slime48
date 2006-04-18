;;; -*- mode: scheme; scheme48-package: restarting -*-

;;;;;; Simple condition restart facility

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type* restarter
  (%make-restarter tag description invoker interactor)
  ())

(define (make-restarter tag description invoker interactor)
  (%make-restarter tag description
                   ((fluid $restarter-invoker-hook) invoker)
                   interactor))

(define-record-discloser :restarter
  (lambda (r) (list 'restarter (restarter-tag r))))

;++ Bletch!  This is an outrageous crock.

(define $restarter-invoker-hook (make-fluid values))

(define (with-restarter-invoker-hook hook thunk)
  (let-fluid $restarter-invoker-hook hook
    thunk))

; (put 'with-restarter-invoker-hook 'scheme-indent-function 1)

(define (restart spec . args)
  (let ((win (lambda (r) (apply (restarter-invoker r) args))))
    (cond ((restarter? spec)
           (win spec))
          ((find-restarter spec)
           => win)
          (else
           (apply restart
                  (error "invalid restarter specifier"
                         `(RESTART ,spec ,@args))
                  args)))))

(define (restart-interactively spec)
  (let ((win (lambda (r)
               (receive args ((restarter-interactor r))
                 (apply (restarter-invoker r) args)))))
    (cond ((restarter? spec)
           (win spec))
          ((find-restarter spec)
           => win)
          (else
           (restart-interactively
            (error "invalid restarter specifier"
                   `(RESTART-INTERACTIVELY ,spec)))))))

(define $restarters (make-fluid '()))

(define (current-restarters) (fluid $restarters))

(define (with-restarter restarter thunk)
  (let-fluid $restarters (cons restarter (fluid $restarters))
    thunk))

; (put 'with-restarter 'scheme-indent-function 1)

;++ This is a crock that does not belong here.

(define (restarters-in-thread thread)
  (let ((saved (get-dynamic-env)))
    (set-dynamic-env! (thread-dynamic-env thread))
    (let ((rs (current-restarters)))
      (set-dynamic-env! saved)
      rs)))

(define (find-restarter tag . restarters)
  (let loop ((restarters (if (pair? restarters)
                             (car restarters)
                             (current-restarters))))
    (cond ((null? restarters)
           #f)
          ((eqv? (restarter-tag (car restarters)) tag)
           (car restarters))
          (else
           (loop (cdr restarters))))))

(define (call-with-restarter tag description invoker receiver)
  (call-with-interactive-restarter tag description invoker #f
    receiver))

(define (call-with-interactive-restarter tag description
            invoker interactor
          receiver)
  (let ((restarter
         (make-restarter tag description invoker interactor)))
    (with-restarter restarter
      (lambda ()
        (receiver restarter)))))

; (put 'call-with-restarter 'scheme-indent-functioon 3)
; (put 'call-with-interactive-restarter 'scheme-indent-function 4)

(define (with-exiting-restarter tag description thunk)
  (call-with-exiting-restarter tag description
    (lambda (r) (thunk))))

(define (call-with-exiting-restarter tag description receiver)
  (call-with-current-continuation
    (lambda (exit)
      (call-with-interactive-restarter
          tag description
          (lambda () (exit))            ; invoker
          (lambda () (values))          ; interactor
        receiver))))

; (put 'with-exiting-restarter 'scheme-indent-function 2)
; (put 'call-with-exiting-restarter 'scheme-indent-function 2)
