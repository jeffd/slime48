;;; -*- mode: scheme; scheme48-package: swank-tcp-servers -*-

;;;;;; SLIME for Scheme48
;;;;;; Swank back end TCP server

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (make-swank-tcp-server . port-number)
  (let ((socket (apply open-socket port-number)))
    (values (socket-port-number socket)
            (make-swank-tcp-server-procedure socket)
            (lambda ()
              (swank-log "Closing server socket on port ~S"
                         (socket-port-number socket))
              (close-socket socket)))))

(define (make-swank-tcp-server-procedure socket)
  (lambda (authenticator winner loser)
    (receive (in out) (socket-accept socket)
      (if (and authenticator
               (not (authenticator in out)))
          (loser)
          (winner
           (lambda (session)
             (let ((reader (spawn-swank-tcp-reader in session))
                   (writer (spawn-swank-tcp-writer out session)))
               (lambda (session)
                 session
                 (terminate-thread! reader)
                 (terminate-thread! writer)
                 (close-input-port in)
                 (close-output-port out)))))))))

(define (spawn-swank-tcp-reader in session)
  (spawn (lambda ()
           (run-swank-tcp-reader in session))
         `(swank-tcp-reader ,(swank-session-id session))))

(define (run-swank-tcp-reader in session)
  (let ((message (decode-swank-message in)))
    ;++ This is a hack.  We can't disconnect in this thread, because
    ;++ disconnecting involves terminating this thread and doing other
    ;++ important things afterward.  So we send a message to the
    ;++ session that we happen to know will cause it to disconnect.
    ;++ This message has null package id, thread id, and return tag.
    (if (eof-object? message)
        (send-incoming-swank-message session
          '(:EMACS-REX (SWANK:QUIT-LISP) NIL NIL NIL))
        (begin (send-incoming-swank-message session message)
               (run-swank-tcp-reader in session)))))

(define (spawn-swank-tcp-writer out session)
  (spawn (lambda ()
           (run-swank-tcp-writer out session))
         `(swank-tcp-writer ,(swank-session-id session))))

(define (run-swank-tcp-writer out session)
  (encode-swank-message (receive-outgoing-swank-message session)
                        out)
  (run-swank-tcp-writer out session))

(define-condition-type 'swank-protocol-error '(error))
(define swank-protocol-error?
        (condition-predicate 'swank-protocol-error))
(define (swank-protocol-error-port error) (caddr error))

(define (decode-swank-message in)
  (if (eof-object? (peek-char in))
      (peek-char in)
      (with-handler (lambda (condition punt)
                      (if (read-error? condition)
                          (signal 'swank-protocol-error
                                  "malformed input expression"
                                  in
                                  condition)
                          (punt)))
        (lambda ()
          (let* ((length (decode-swank-message-length in))
                 (string (make-string length))
                 (count-read (read-block string 0 length in)))
            (if (or (eof-object? count-read)
                    (< count-read length))
                (signal 'swank-protocol-error
                        "premature end of file"
                        in)
                (read-from-string string)))))))

(define (decode-swank-message-length port)
  (do ((c 0 (+ c 1))
       (i 0 (bitwise-ior (arithmetic-shift i 4)
                         (read-hex-digit port))))
      ((= c 6) i)))

(define (encode-swank-message message out)
  (let* ((string (write-to-string message)))
    (encode-swank-message-length string out)
    (write-string string out)
    (force-output out)))

(define (encode-swank-message-length string out)
  (let ((length (string-length string)))
    (if (>= length (arithmetic-shift 2 24))
        (error "message too long to be encoded in Swank" string)
        (let ()
          (define (extract-digit n)
            (bitwise-and (arithmetic-shift length (* -4 n))
                         #xF))
          (define (write-digit n)
            (write-hex-digit (extract-digit n) out))
          (write-digit 5)
          (write-digit 4)
          (write-digit 3)
          (write-digit 2)
          (write-digit 1)
          (write-digit 0)))))

(define (read-hex-digit port)
  (let ((char (read-char port)))
    (if (eof-object? char)
        (signal 'read-error
                "premature end of file"
                port)
        (let ((ascii (char->ascii char)))
          (cond ((char-between? #\0 char #\9)
                 (- ascii (char->ascii #\0)))
                ((char-between? #\a char #\f)
                 (+ 10 (- ascii (char->ascii #\a))))
                ((char-between? #\A char #\F)
                 (+ 10 (- ascii (char->ascii #\A))))
                (else
                 (signal 'read-error
                         "invalid hex digit"
                         char
                         port)))))))

(define (char-between? lower char upper)
  ;; Why isn't CHAR<=? n-ary?
  (and (char<=? lower char)
       (char<=? char upper)))

(define (write-hex-digit digit port)
  (cond ((<= 0 digit 9)
         (write-char (ascii->char (+ digit (char->ascii #\0)))
                     port))
        ((<= 10 digit 16)
         (write-char (ascii->char (+ (- digit 10) (char->ascii #\A)))
                     port))
        (else
         (call-error "invalid hex digit"
                     write-hex-digit digit port))))
