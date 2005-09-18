;;; -*- mode: scheme; scheme48-package: swank-i/o -*-

;;;;;; SLIME for Scheme48
;;;;;; Ports instrumented for I/O with a remote Swank system

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; Output ports: a simple wrapper over Scheme48's built-in buffered
;;; ports

(define (make-swank-output-port session-placeholder)
  (make-buffered-output-port swank-output-port-handler
                             session-placeholder
                             (make-byte-vector default-buffer-size 0)
                             0          ; index
                             default-buffer-size))

(define swank-output-port-handler
        (make-buffered-output-port-handler
         (lambda (session-placeholder)  ; discloser
           (list 'swank-output-port
                 (swank-session-id
                  (placeholder-value session-placeholder))))
         (lambda (port)                 ; closer
           (error "should not be closing an Swank output port"
                  port))
         (lambda (port necessary?)      ; buffer emptier
           (empty-swank-output-buffer port))
         (lambda (port)                 ; readiness tester
           (if (maybe-commit)
               (values #t #t)
               (values #f #f)))))

(define (empty-swank-output-buffer port)
  (let* ((size (provisional-port-index port))
         (string (make-string size)))
    (copy-bytes! (port-buffer port) 0
                 string             0 size)
    (provisional-set-port-index! port 0)
    (if (maybe-commit)
        (begin (send-outgoing-swank-message
                   (placeholder-value (port-data port))
                 `(:READ-OUTPUT ,string))
               #t)
        #f)))



;;; Input ports are somewhat more complicated; because of the fixed
;;; buffer in a port, we can't just repeatedly receive new buffers from
;;; the remote system to set the buffer field to when we get them, so
;;; we elide the built-in buffering system entirely.

(define (make-swank-input-port session-placeholder)
  (make-unbuffered-input-port swank-input-port-handler
                              (make-swank-input session-placeholder)))

(define-record-type* swank-input
  (make-swank-input session-placeholder)
  ((lock (make-lock))
   (buffer #f)
   (index #f)))

(define (swank-input-session data)
  (placeholder-value (swank-input-session-placeholder data)))

(define swank-input-port-handler
        (make-port-handler
         (lambda (port)                 ; discloser
           (list 'swank-input-port
                 (swank-session-id
                  (swank-input-session (port-data port)))))
         (lambda (port)                 ; closer
           (error "should not be closing a Swank input port"
                  port))
         (lambda (port consume?)        ; character reader
           (swank-read-char port consume?))
         (lambda (port target tstart count wait?)
           (swank-read-block port target tstart count wait?))
         (lambda (port)
           (and (swank-input-buffer (port-data port))
                #t))
         #f))                           ; forcer

(define (swank-read-char port consume?)
  (let* ((data (port-data port))
         (lock (swank-input-lock data)))
    (dynamic-wind
      (lambda () (obtain-lock lock))
      (lambda ()
        (receive (buffer index) (swank-input-buffer+index data)
          (let ((char (string-ref buffer index)))
            (if consume?
                (if (= (+ index 1) (string-length buffer))
                    (begin (set-swank-input-buffer! data #f)
                           (set-swank-input-index! data #f))
                    (set-swank-input-index! data (+ index 1))))
            char)))
      (lambda () (release-lock lock)))))

(define (swank-input-buffer+index data)
  (cond ((swank-input-buffer data)
         => (lambda (buffer)
              (values buffer (swank-input-index data))))
        (else
         (let ((buffer (request-swank-input
                        (swank-input-session data))))
           (set-swank-input-buffer! data buffer)
           (set-swank-input-index! data 0)
           (values buffer 0)))))

(define (swank-read-block port target tstart count wait?)
  (let ((data (port-data port))
        (tend (+ tstart count)))
    ((lambda (body)
       (let ((lock (swank-input-lock data)))
         (dynamic-wind
           (lambda () (obtain-lock lock))
           body
           (lambda () (release-lock lock)))))
     (lambda ()
       (define (copy buffer bindex tindex stored?)
         (let ((bend (string-length buffer)))
           (cond ((>= (- bend bindex)
                      (- tend tindex))
                  (copy-bytes! buffer bindex
                               target tindex
                               (- tend tindex))
                  (if (not stored?)
                      (begin (set-swank-input-buffer! data buffer)
                             (set-swank-input-index!
                              data
                              (+ bindex (- tend tindex)))))
                  count)
                 (else
                  (copy-bytes! buffer bindex
                               target tindex
                               (- bend bindex))
                  (if stored?
                      (begin (set-swank-input-buffer! data #f)
                             (set-swank-input-index! data #f)))
                  (wait (+ tindex (- bend bindex)))))))

       (define (wait tindex)
         (if wait?
             (copy (request-swank-input (swank-input-session data))
                   0
                   tindex
                   #f)
             (- tend tindex)))

       (cond ((swank-input-buffer data)
              => (lambda (buffer)
                   (copy buffer (swank-input-index data)
                         tstart
                         #t)))
             (else (wait tstart)))))))
