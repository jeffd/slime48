;;; -*- mode: scheme; scheme48-package: swank-sldb -*-

;;;;;; SLIME for Scheme48
;;;;;; SLDB (SLIME Debugger) back end

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;++ There is considerable potential for better abstraction of much of
;++ this file.

(define (with-sldb-handler filter thunk)
  (with-handler (let ((filter (or filter error?)))
                  (lambda (condition punt)
                    (if (or (filter condition)
                            (breakpoint? condition))
                        (sldb-condition-handler condition punt)
                        (punt))))
    thunk))

(define breakpoint? (condition-predicate 'BREAKPOINT))

; (put 'with-sldb-handler 'scheme-indent-function 1)

(define (swank:simple-break)
  (with-exiting-restarter 'continue "Continue from break."
    (lambda ()
      (sldb-condition-handler '(BREAKPOINT)
                              ;; Nothing to punt.
                              values)))
  'nil)

(define sldb-condition-handler
        (lambda (condition punt)
          ((call-with-current-continuation
            (lambda (k)
              (lambda ()
                (let-fluid $sldb-condition-punter (lambda () (k punt))
                  (lambda ()
                    (push-swank-level condition
                      send-sldb-activation
                      send-sldb-return)))))))))

(define (send-sldb-activation)
  (send-outgoing-swank-message (current-swank-session)
    `(:DEBUG ,(thread-uid (sldb-thread-to-debug))
             ,(sldb-level)
             ,(sldb-disclose-condition)
             ,(sldb-restarter-listing)
             ,(swank:backtrace 0 20)    ; [0,20) - same as in CL Swank
             ,(sldb-pending-return-tags))))

(define (sldb-restarter-listing)
  (map (lambda (restarter)
         (list (string-upcase           ; CL convention
                (circular-write-to-string (restarter-tag restarter)))
               (restarter-description restarter)))
       (sldb-restarters)))

(define (send-sldb-return)
  (send-outgoing-swank-message (current-swank-session)
    `(:DEBUG-RETURN ,(thread-uid (sldb-thread-to-debug))
                    ,(sldb-level)
                    ;; NIL = false -> not stepping
                    NIL)))

(define (sldb-level)
  (swank-session-level-number (current-swank-session)))

(define (sldb-thread-to-debug)
  (swank-session-pusher-thread (current-swank-session)))

(define (sldb-restarters)
  ;; We reverse this to produce a more useful listing for SLDB: put the
  ;; most relevant restarters closer to the backtrace, which is what
  ;; the SLDB window focusses on, but the numbers start at 0 near the
  ;; top, so we put the general restarters up there, such as those to
  ;; reset or resume the top level.
  (reverse (restarters-in-thread (sldb-thread-to-debug))))

(define (call-with-sldb-continuation recipient)
  (with-ignorable-frame-template
      (closure-template (loophole :closure push-swank-level))
    (lambda ()
      (recipient (thread-continuation (sldb-thread-to-debug))))))

(define (sldb-pending-return-tags)
  (swank-session-pending-return-tags (current-swank-session)))

(define (sldb-condition)
  (swank-session-condition (current-swank-session)))

(define (sldb-disclose-condition)
  (let ((condition (sldb-condition)))
    (list (sldb-condition-string condition)
          (string-append "[Condition of type "
                         (string-upcase   ; CL convention
                          (symbol->string (condition-type condition)))
                         "]")
          '()                           ; manual references
          '())))                        ; extras (?)

(define (string-upcase string)          ;++ cheesy hack -- remove
  (let* ((len (string-length string))
         (result (make-string len)))
    (do ((i 0 (+ i 1)))
        ((= i len) result)
      (string-set! result i (char-upcase (string-ref string i))))))

(define (sldb-condition-string condition)
  (call-with-current-continuation
    (lambda (k)
      (with-handler
          (lambda (c punt)
            (if (error? c)
                (with-handler (lambda (c punt)
                                (if (error? c)
                                    (k "Unable to display condition")
                                    (punt)))
                  (lambda ()
                    (limited-write-to-string condition)))
                (punt)))
        (lambda ()
          (call-with-string-output-port
            (lambda (port)
              (display-condition condition port))))))))

(define $sldb-condition-punter (make-fluid #f))

(define (swank:sldb-break-with-default-debugger)
  ((fluid $sldb-condition-punter))
  'nil)

(define (swank:backtrace start end)
  (call-with-sldb-continuation
   (lambda (cont)
     (let loop ((cont cont)
                (i 0))
       (cond ((not cont) '())
             ((= i start)
              (continuation-frame-list cont start
                                       (and end (- end start))))
             (else
              (loop (continuation-cont cont)
                    (if (ignorable-frame? cont)
                        i
                        (+ i 1)))))))))

(define (swank:debugger-info-for-emacs start end)
  (list (sldb-disclose-condition)
        (sldb-restarter-listing)
        (swank:backtrace start end)
        (sldb-pending-return-tags)))

(define (swank:invoke-nth-restart-for-emacs level n)
  (let* ((session (current-swank-session))
         (current-level (swank-session-level-number session)))
    (if (= level current-level)
        (let loop ((restarters (sldb-restarters))
                   (n n))
          (cond ((null? restarters)
                 (abort-swank-rpc
                  "(session ~S, level ~S) SLDB restarter index ~S ~A"
                  (swank-session-id session)
                  current-level
                  n
                  "out of bounds"))
                ((zero? n)
                 (restart-interactively (car restarters)))
                (else
                 (loop (cdr restarters)
                       (- n 1)))))
        ;; Silently ignore the request if it's on the wrong level.
        ;++ Why silently?  Why not abort?
        'nil)))

(define (sldb-restart tag restarters)
  (cond ((find-restarter tag restarters)
         => (lambda (r)
              (restart r)))
        (else
         (let ((session (current-swank-session)))
           (abort-swank-rpc
            "(session ~S, level ~S) No ~A restarter"
            (swank-session-id session)
            (swank-session-level-number session)
            (string-upcase (circular-write-to-string tag)))))))

;;; Be very careful here about the ordering of the restarters:
;;; SLDB-RESTARTERS returns them in order from outermost to innermost.
;;; SLDB-ABORT and SLDB-CONTINUE want to use the innermost restarters.
;;; THROW-TO-TOP-LEVEL wants to use the outermost restarter.

(define (swank:sldb-abort)
  (sldb-restart 'ABORT (reverse (sldb-restarters))))

(define (swank:sldb-continue)
  (sldb-restart 'CONTINUE (reverse (sldb-restarters))))

;;; This is very different from what the CL Swank back end does.  I
;;; don't understand how that works, though.  This just picks out the
;;; last RESET restarter, which should be the one to the top level.

(define (swank:throw-to-toplevel)
  (sldb-restart 'RESET (sldb-restarters)))

(define (swank:inspect-current-condition)
  (inspect-object (sldb-condition)))

(define (swank:inspect-frame-var frame-number var-number)
  (cond ((and-let* ((frame (call-with-sldb-continuation
                             (lambda (cont)
                               (continuation-frame-ref cont
                                                       frame-number))))
                    (locals (frame-locals-list frame
                                               (lambda (name value)
                                                 name
                                                 value)))
                    ((< var-number (length locals))))
           (list-ref locals var-number))
         => inspect-object)
        (else
         (let ((session (current-swank-session)))
           (abort-swank-rpc
            "(session ~S, level ~S) No such frame variable (~S, ~S)"
            (swank-session-id session)
            (swank-session-level-number session)
            frame-number
            var-number)))))

(define (swank:inspect-in-frame string n)
  (cond ((eval-in-sldb-frame n string)
         => inspect-results)
        (else
         (let ((session (current-swank-session)))
           (abort-swank-rpc
            "(session ~S, level ~S) No expression to inspect: ~S"
            (swank-session-id session)
            (swank-session-level-number session)
            string)))))

(define (swank:eval-string-in-frame string n)
  (eval-in-sldb-frame* n string
    (lambda () "; Nothing to evaluate")
    (lambda () "; No values")
    (lambda (v)
      (limited-write-to-string v))
    (lambda (vals)
      (delimited-object-list-string vals limited-write ","))))

(define (swank:pprint-eval-string-in-frame string n)
  (eval-in-sldb-frame* n string
    (lambda () "; Nothing to evaluate")
    (lambda () "; No values")
    (lambda (v) (pp-to-string v))
    (lambda (vals)
      (delimited-object-list-string vals p ""))))

(define (eval-in-sldb-frame n string)
  (cond ((sldb-frame-ref n)
         => (lambda (frame)
              (let ((exp (read-from-string string)))
                (if (eof-object? exp)
                    #f
                    (receive results (eval-in-frame exp frame)
                      results)))))
        (else
         (repl-eval-string string))))

(define (eval-in-sldb-frame* n string nothing zero one many)
  (let ((results (eval-in-sldb-frame n string)))
    (cond ((not results) (nothing))
          ((null? results) (zero))
          ((null? (cdr results)) (one (car results)))
          (else (many results)))))

; (put 'eval-in-sldb-frame* 'scheme-indent-function 2)

;;; No such thing as a catch tag in Scheme.

(define (swank:frame-catch-tags-for-emacs n) '())

(define (swank:frame-locals-for-emacs n)
  (or (and-let* ((frame (sldb-frame-ref n)))
        (frame-locals-list frame make-frame-local-for-emacs))
      '()))

(define (make-frame-local-for-emacs name value)
  `(:NAME ,(cond ((not name)
                  "(anonymous)")
                 ((symbol? name)
                  (symbol->string name))
                 ((generated? name)
                  ;; Take the symbol of the parent; we separate the id
                  ;; out here manually.
                  (name->symbol (generated-name name)))
                 (else                  ; bizarre name
                  (limited-write-to-string name)))
    :ID ,(if (generated? name)
             (generated-uid name)
             0)
    :VALUE ,(limited-write-to-string value)))

(define (swank:sldb-disassemble n)
  (cond ((sldb-frame-ref n)
         => (lambda (frame)
              (with-output-to-string
                (lambda ()
                  (display "* PC: ")      ;++ ugly hack
                  (write (continuation-pc frame))
                  (newline)
                  (disassemble frame)))))
        (else
         (let ((session (current-swank-session)))
           (abort-swank-rpc
            "(session ~S, level ~S) No such frame to disassemble: ~S"
            (swank-session-id session)
            (swank-session-level-number session)
            n)))))

(define (swank:frame-source-location-for-emacs n)
  (or (and-let* ((frame (sldb-frame-ref n))
                 (template (continuation-template frame)))
        (template-source-location template
                                  (continuation-pc frame)))
      `(:ERROR ,(string-append "No source location for frame "
                               (number->string n 10)))))

(define (sldb-frame-ref n)
  (call-with-sldb-continuation
   (lambda (cont)
     (let ((frame (continuation-frame-ref cont n)))
       (if (continuation? frame)
           frame
           #f)))))
