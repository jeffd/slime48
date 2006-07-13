;;; -*- mode: scheme; scheme48-package: swank-tcp-servers -*-

;;;;;; SLIME for Scheme48
;;;;;; Swank back end TCP server

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (make-one-shot-swank-tcp-server world . port-number)
  (let* ((socket (apply open-socket port-number))
         (port-number (socket-port-number socket)))
    (swank-log "(world ~S) Spawning one-shot TCP server on port ~A"
               (swank-world-id world)
               (socket-port-number socket))
    (values port-number
            (lambda (session-wrapper)
              (receive (in out) (socket-accept socket)
                (close-socket socket)
                (spawn-swank-tcp-session in out world
                                         session-wrapper))))))

(define-record-type* swank-tcp-server
  (make-swank-tcp-server world socket)
  (thread))

(define (spawn-swank-tcp-server world session-wrapper . port-number)
  (let ((socket (apply open-socket port-number)))
    (swank-log "(world ~S) Spawning TCP server on port ~A"
               (swank-world-id world)
               (socket-port-number socket))
    (let* ((server (make-swank-tcp-server world socket))
           (thread (spawn (lambda ()
                            (run-swank-tcp-server server
                                                  session-wrapper))
                          `(swank-tcp-server
                            ,(swank-world-id world)
                            ,(socket-port-number socket)))))
      (set-swank-tcp-server-thread! server thread)
      server)))

(define (close-swank-tcp-server server)
  (let ((socket (swank-tcp-server-socket server)))
    (swank-log "(world ~S) Closing TCP server on port ~A"
               (swank-world-id (swank-tcp-server-world server))
               (socket-port-number socket))
    (terminate-thread! (swank-tcp-server-thread server))
    (close-socket socket)))

(define (swank-tcp-server-port-number server)
  (socket-port-number (swank-tcp-server-socket server)))

(define (run-swank-tcp-server server session-wrapper)
  (let ((socket (swank-tcp-server-socket server))
        (world (swank-tcp-server-world server)))
    (let loop ()
      (receive (in out) (socket-accept socket)
        (spawn-swank-tcp-session in out world session-wrapper))
      (loop))))

(define (spawn-swank-tcp-session in out world session-wrapper)
  (let ((session-placeholder (make-placeholder))
        (reader-placeholder (make-placeholder))
        (writer-placeholder (make-placeholder)))
    (let ((session
           (session-wrapper session-placeholder
             (lambda (init exit)
               (spawn-swank-session world
                 ;; These next two procedures will be called in other
                 ;; threads, so we must be careful about synchronizing
                 ;; access to the session descriptor and the two I/O
                 ;; thread descriptors.
                 (lambda ()             ; session winder
                   (spawn-swank-tcp-i/o
                    in out
                    (placeholder-value session-placeholder)
                    reader-placeholder
                    writer-placeholder)
                   (init))
                 (lambda ()             ; session unwinder
                   (exit)
                   (terminate-thread!
                    (placeholder-value reader-placeholder))
                   (terminate-thread!
                    (placeholder-value writer-placeholder))
                   (close-input-port in)
                   (close-output-port out)))))))
      (placeholder-set! session-placeholder session))))

(define (spawn-swank-tcp-i/o in out session
                             reader-placeholder
                             writer-placeholder)
  (let ((reader (spawn (lambda ()
                         (run-swank-tcp-reader session in))
                       `(swank-tcp-reader ,session)))
        (writer (spawn (lambda ()
                         (run-swank-tcp-writer session out))
                       `(swank-tcp-writer ,session))))
    (placeholder-set! reader-placeholder reader)
    (placeholder-set! writer-placeholder writer)))

(define (run-swank-tcp-reader session in)
  (let loop ()
    (send-incoming-swank-message session (decode-swank-message in))
    (loop)))

(define-condition-type 'swank-protocol-error '(error))
(define swank-protocol-error?
        (condition-predicate 'swank-protocol-error))
(define (swank-protocol-error-port error) (caddr error))

(define (decode-swank-message in)
  (let* ((length (decode-swank-message-length in))
         (string (make-string length))
         (count-read (read-block string 0 length in)))
    (if (not (= count-read length))
        (signal 'swank-protocol-error
                "number of bytes read does not match expected length"
                in
                `(expected ,length)
                `(read ,count-read)))
    (with-handler (lambda (condition punt)
                    (if (read-error? condition)
                        (signal 'swank-protocol-error
                                "malformed input expression"
                                in
                                condition)
                        (punt)))
      (lambda ()
        (read-from-string string)))))

(define (decode-swank-message-length port)
  (do ((c 0 (+ c 1))
       (i 0 (bitwise-ior (arithmetic-shift i 4)
                         (read-hex-digit port))))
      ((= c 6) i)))

(define (read-hex-digit port)
  (let ((char (read-char port)))
    (if (eof-object? char)
        (signal 'swank-protocol-error
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
                 (signal 'swank-protocol-error
                         "invalid hex digit for message length read"
                         port
                         char)))))))

(define (char-between? lower char upper)
  ;; Why isn't CHAR<=? n-ary?
  (and (char<=? lower char)
       (char<=? char upper)))

(define (run-swank-tcp-writer session out)
  (let loop ()
    (encode-swank-message (receive-outgoing-swank-message session)
                          out)
    (loop)))

(define (encode-swank-message message out)
  (let* ((string (write-to-string message)))
    (write-string (encode-swank-message-length string) out)
    (write-string string out)
    (write-char #\newline out)          ; Careful not to call NEWLINE!
    (force-output out)))

(define (encode-swank-message-length string)
  (let ((length (+ (string-length string) 1)))
    (if (>= length (arithmetic-shift 2 24))
        (error "message too long to be encoded in Swank" string)
        (let ((hex (number->string length 16)))
          (string-append (make-string (- 6 (string-length hex)) #\0)
                         hex)))))
