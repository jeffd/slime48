;;; -*- mode: scheme; scheme48-package: swank-sessions -*-

;;;;;; SLIME for Scheme48
;;;;;; Swank sessions

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type* swank-session
  (make-swank-session world id)
  (controller-thread
   scheduler-thread
   (repl-pipe (make-pipe))              ; controller sends REPL eval
                                        ;   requests here
   (in-pipe   (make-pipe))              ; remote system sends messages
                                        ;   for the local system here
   (out-pipe  (make-pipe))              ; local system sends messages
                                        ;   for the remote system here
   (input-placeholders
    (make-weak-table abs))              ; table mapping input tags to
                                        ;   response placeholders
   (input-request-lock (make-lock))     ; lock for above table
   level                                ; current interaction level
   (data (make-integer-table))          ; table of arbitrary data
   top-dynamic-env                      ; for resetting the top level
   ))

(define-record-discloser :swank-session
  (lambda (session)
    (list 'swank-session (swank-session-id session))))

(define (spawn-swank-session world top-winder top-unwinder)
  (let ((id (next-swank-session-id world)))
    (swank-log "(world ~S) Spawning session ~S"
               (swank-world-id world)
               id)
    (let ((session (make-swank-session world id)))
      (spawn-swank-session-controller session)
      (spawn-swank-session-levels session top-winder top-unwinder)
      (weak-table-set! (swank-world-sessions world) id session)
      session)))

; (put 'spawn-swank-session 'scheme-indent-function 2)

(define (send-incoming-swank-message session message)
  (pipe-write! (swank-session-in-pipe session) message))

(define (receive-incoming-swank-message session)
  (let ((message (pipe-read! (swank-session-in-pipe session))))
    (swank-log "(session ~S) Receiving incoming message ~S"
               (swank-session-id session) message)
    message))

(define (send-outgoing-swank-message session message)
  (swank-log "(session ~S) Sending outgoing message ~S"
             (swank-session-id session) message)
  (pipe-write! (swank-session-out-pipe session) message))

; (put 'send-outgoing-swank-message 'scheme-indent-function 1)

(define (receive-outgoing-swank-message session)
  (pipe-read! (swank-session-out-pipe session)))



;;; ----------------
;;; Swank session data

(define *swank-session-slot-index* 0)

(define (current-swank-session-data)
  (swank-session-data (current-swank-session)))

(define (make-swank-session-slot initializer)
  (let ((index *swank-session-slot-index*))
    (set! *swank-session-slot-index* (+ index 1))
    (values (lambda ()
              (let ((data (current-swank-session-data)))
                (cond ((table-ref data index)
                       => cell-ref)
                      (else
                       (let ((value (initializer)))
                         (table-set! data index (make-cell value))
                         value)))))
            (lambda (value)             ; Setter, not really necessary.
              (table-set! (swank-session-data (current-swank-session))
                          index
                          (make-cell value)))
            (lambda (modifier)
              (let ((data (current-swank-session-data)))
                (cond ((table-ref data index)
                       => (lambda (cell)
                            (cell-set! cell
                                       (modifier (cell-ref cell)))))
                      (else
                       (table-set! data index
                                   (make-cell
                                    (modifier (initializer)))))))))))

(define-syntax define-swank-session-slot
  (syntax-rules ()
    ((DEFINE-SWANK-SESSION-SLOT accessor-name
       setter-name
       modifier-name
       initializer)
     (BEGIN (DEFINE accessor-name)
            (DEFINE setter-name)
            (DEFINE modifier-name)
            (RECEIVE (ACCESSOR SETTER MODIFIER)
                     (MAKE-SWANK-SESSION-SLOT (LAMBDA () initializer))
              (SET! accessor-name ACCESSOR)
              (SET! setter-name SETTER)
              (SET! modifier-name MODIFIER))))))



;;; ----------------
;;; Session controller: receives messages from the remote system and
;;; operates accordingly on the session

(define (spawn-swank-session-controller session)
  (let ((thread (spawn (lambda ()
                         (control-swank-session session))
                       `(swank-session-controller
                         ,(swank-session-id session)))))
    (set-swank-session-controller-thread! session thread)))

(define (control-swank-session session)
  (let ((message (receive-incoming-swank-message session)))
    (destructure-case message
      ((:EMACS-REX form package-id thread-id return-tag)
       ;; Brazenly ignore values of THREAD-ID other than :REPL-THREAD.
       ;; The way the CL SLIME uses this argument is incompatible with
       ;; how SLIME48 works, and it's unnecessary anyway.
       (if (eq? thread-id ':repl-thread)
           (eval-in-repl-thread form package-id return-tag session)
           (eval-in-worker-thread form package-id return-tag session)))
      ((:EMACS-INTERRUPT thread-id)
       (cond ((swank-thread-for-id thread-id session)
              => interrupt-swank-thread)
             (else
              (warn "no such thread in session to interrupt"
                    message session))))
      ((:EMACS-RETURN thread-id tag value)
       (error "Swank :EMACS-RETURN message unimplemented"
              message))
      ((:EMACS-RETURN-STRING thread-id tag value)
       (return-swank-input tag value session))
      (else
       (warn "unrecognized Swank session control message"
             message session))))
  (control-swank-session session))

;++ Some day, threads should be selectable by symbolic name.

(define (swank-thread-for-id thread-id session)
  (cond ((eq? thread-id ':repl-thread)
         (swank-session-repl-thread session))
        ((exact-nonnegative-integer? thread-id)
         (level-thread-for-id (swank-session-level session)
                              thread-id))
        (else #f)))

(define (exact-nonnegative-integer? obj)
  (and (integer? obj)
       (exact? obj)
       (<= 0 obj)))

(define (interrupt-swank-thread thread)
  (interrupt-thread thread
    (lambda args
      (with-exiting-restarter 'continue "Continue from breakpoint."
        (lambda ()
          (apply signal 'breakpoint "Swank user interrupt"
                 args)))
      (apply values args))))

(define-swank-session-slot swank-input-tag
  set-swank-input-tag!
  modify-swank-input-tag!
  0)

(define (request-swank-input session)
  (let* ((placeholder (make-placeholder))
         (lock (swank-session-input-request-lock session))
         (tag (dynamic-wind
                (lambda () (obtain-lock lock))
                (lambda ()
                  (let ((tag (swank-input-tag)))
                    (set-swank-input-tag! (+ tag 1))
                    (table-set! (swank-session-input-placeholders
                                 session)
                                tag
                                placeholder)
                    tag))
                (lambda () (release-lock lock)))))
    (send-outgoing-swank-message session
      `(:READ-STRING T     ; This must not be nil.  I don't know why.
                     ,tag))
    (placeholder-value placeholder)))

(define (return-swank-input tag value session)
  (let ((lock (swank-session-input-request-lock session)))
    (dynamic-wind
      (lambda () (obtain-lock lock))
      (lambda ()
        (let ((table (swank-session-input-placeholders session)))
          (cond ((table-ref table tag)
                 => (lambda (placeholder)
                      (placeholder-set! placeholder value)
                      (table-set! table tag #f)))
                (else
                 (warn "no such Swank input tag returned to from Emacs"
                       tag value session)))))
      (lambda () (release-lock lock)))))



;;; ----------------
;;; Evaluation in threads

;;; EVAL-IN-WORKER-THREAD spawns a new thread to run a Swank RPC
;;; expression; EVAL-IN-REPL-THREAD sends the RPC expression to the
;;; REPL thread's pipe; and EVAL-IN-INTERRUPTED-THREAD interrupts a
;;; running thread to evaluate it and then proceed with whatever it was
;;; doing.  SWANK-EVAL is the core of all this to evaluate the RPC
;;; expression.

(define (eval-in-worker-thread form package-id return-tag session)
  (spawn-on-scheduler (swank-session-scheduler-thread session)
    (lambda ()
      (swank-eval form package-id return-tag session))
    ;; Give it a useful identifier: the Emacs user has to be able to
    ;; identify what worker he wants to operate, and the only data he
    ;; has on which to base this identification is the form to evaluate
    ;; (but the return tag might be useful to).
    `(swank-session-worker ,(swank-session-id session)
                           ,return-tag ,form)))

(define (eval-in-repl-thread form package-id return-tag session)
  (pipe-write! (swank-session-repl-pipe session)
               (list form package-id return-tag)))

(define (eval-in-interrupted-thread thread form package-id
                                    return-tag session)
  (interrupt-thread thread
    (lambda args
      (swank-eval form package-id return-tag session)
      (apply values args))))

;;; SWANK-EVAL -- Evaluate a remote expression in the Swank package,
;;; binding the interaction environment to the environment with the
;;; given package specifier.  If the expression changes the interaction
;;; environment, reflect the change outside; otherwise, revert the
;;; interaction environment to whatever it was before.

;;; Call this in a thread running under a Swank session scheduler so
;;; that it has the right condition handler.

;++ This naming scheme is horrible: SWANK-EVAL, *SWANK-EVAL,
;++ **SWANK-EVAL, EVAL-IN-RPC-ENV

(define (swank-eval form package-id return-tag session)
  ;; The package system sentinel must be invoked before any possible
  ;; environment operations.
  (package-system-sentinel)
  (with-swank-evaluation session return-tag
    (lambda (result-cell)
      (*swank-eval form package-id session result-cell))))

(define (with-swank-evaluation session return-tag body)
  (call-with-current-continuation
    (lambda (escape)
      (let ((result-cell (make-cell '(:ABORT)))
            (already-returned? (make-cell #f)))
        (with-swank-abort-restarter result-cell escape
          (lambda ()
            (let-fluid $swank-return-tag return-tag
              (lambda ()
                (dynamic-wind
                  values                ;++ Guard against re-entrance?
                  (lambda ()
                    (body result-cell))
                  (lambda ()
                    (swank-return result-cell
                                  already-returned?
                                  return-tag
                                  session)))))))))))

(define (with-swank-abort-restarter result-cell escape body)
  (call-with-interactive-restarter
      'ABORT     ;++ Should this perhaps be ABORT-REQUEST or something?
      "Abort SLIME request."
      (lambda format+arguments          ;Invoker
        (cell-set!
         result-cell
         (cond ((pair? format+arguments)
                (apply swank-log format+arguments)
                `(:ABORT ,(apply format #f format+arguments)))
               (else
                '(:ABORT))))
        (escape))
      (lambda ()                        ;Interactor
        (values))
    (lambda (r)
      (let-fluid $swank-rpc-aborter r
        body))))

(define (swank-return result-cell already-returned? return-tag session)
  (cond ((cell-ref already-returned?)
         (warn "ignoring second return from SLIME request"
               return-tag))
        (else
         (cell-set! already-returned? #t)
         (send-outgoing-swank-message session
           `(:RETURN ,(cell-ref result-cell)
                     ,return-tag)))))

(define (*swank-eval form package-id session result-cell)
  (cond ((eq? package-id 'nil)
         (cell-set! result-cell
                    `(:OK ,(eval-in-rpc-env form session))))
        ((find-package-in-swank-world
          (if (string? package-id)
              (maybe-read-from-string package-id)
              package-id)
          (swank-session-world session))
         => (lambda (env)
              (receive (value new-env)
                       (**swank-eval form env session)
                (if (not (eq? new-env env))
                    (set-interaction-environment! new-env))
                (cell-set! result-cell `(:OK ,value)))))
        ;++ What to do if we can't find the package?  Just use the
        ;++ interaction environment?
        (else
         (swank-log "(session ~S) No such package ~S for ~A ~S"
                    (swank-session-id session)
                    package-id
                    "interaction environment in Swank evaluation"
                    form))))

(define (**swank-eval form env session)
  (with-interaction-environment env
    (lambda ()
      (let ((value (eval-in-rpc-env form session)))
        ;** Don't beta-reduce!  INTERACTION-ENVIRONMENT must be called
        ;** _after_ the form is evaluated.
        (values value (interaction-environment))))))

(define (eval-in-rpc-env form session)
  (eval form (swank-world-rpc-env (swank-session-world session))))

(define $swank-return-tag (make-fluid #f))
(define (current-swank-return-tag) (fluid $swank-return-tag))

(define $swank-rpc-aborter (make-fluid #f))
(define (abort-swank-rpc . why)
  (apply restart (fluid $swank-rpc-aborter) why))

(define (maybe-read-from-string string)
  (call-with-current-continuation
    (lambda (lose)
      (with-handler (lambda (condition punt) (lose))
        (lambda ()
          (read-from-string string))))))



;;; ----------------
;;; Swank levels

(define-record-type* level
  (%make-level parent                   ; level that pushed this one
               continuation             ; continuation of pusher, for
                                        ;   exiting the level
               return-tag               ; pending Swank RPC return tag
               condition                ; condition that caused this
                                        ;   level to be pushed, or #F
               pusher-thread            ; thread that pushed this level
                                        ;   (or #F if this is the top)
               dynamic-env              ; dynamic environment for new
                                        ;   threads on this level
               session                  ; session this level is running
                                        ;   on
               ;; These two need to be fields for RESET-SWANK-LEVEL,
               ;; which has to have access to them to construct the
               ;; call to *PUSH-SWANK-LEVEL to duplicate the original
               ;; level.  They are not used otherwise.
               winder                   ; thunk to call on entering
               unwinder                 ; thunk to call on exiting
               )
  ((number (if parent (+ 1 (level-number parent)) 0))
   (repl-thread #f)                     ; thread that reads from the
                                        ;   REPL pipe
   (child #f)                           ; the next level, if one was
                                        ;   pushed from this one
   (threads (make-weak-table))          ; table from uids to threads
                                        ;   running on this level
   (queue (make-queue))                 ; queue of threads to run
   (counter (make-counter))             ; counter for scheduler
   ))

(define-record-discloser :level
  (lambda (level)
    (list 'level (level-number level) (level-session level))))

(define (make-level parent continuation return-tag
                    condition pusher-thread dynamic-env
                    session winder unwinder)
  (let ((level (%make-level parent continuation return-tag
                            condition pusher-thread dynamic-env
                            session winder unwinder)))
    (spawn-swank-repl level)
    level))

(define     thread-level      thread-data)
(define set-thread-level! set-thread-data!)

(define (level-thread-for-id level uid)
  (weak-table-ref (level-threads level) uid))

(define (walk-level-threads level proc)
  (weak-table-walk (level-threads level)
                   (lambda (uid thread) (proc thread))))

(define (level-thread-count level)
  (counter-value (level-counter level)))

(define (spawn-on-level thread level)
  (set-thread-dynamic-env! thread (level-dynamic-env level))
  (set-thread-scheduler! thread (swank-session-scheduler-thread
                                 (level-session level)))
  (set-thread-level! thread level)
  (weak-table-set! (level-threads level)
                   (thread-uid thread)
                   thread)
  (enqueue! (level-queue level) thread)
  (increment-counter! (level-counter level)))

(define (spawn-swank-repl level)
  (let ((thread (make-thread (lambda ()
                               (run-swank-repl level))
                             `(swank-repl
                               ,(let ((n (level-number level)))
                                  (if (zero? n) 'top-level n))
                               ,(swank-session-id
                                 (level-session level))))))
    (spawn-on-level thread level)
    (set-level-repl-thread! level thread)))

(define (run-swank-repl level)
  (let* ((session (level-session level))
         (pipe (swank-session-repl-pipe session)))
    (let loop ()
      (destructure (((form package-id return-tag) (pipe-read! pipe)))
        (swank-eval form package-id return-tag session))
      (loop))))

;;; Exported operations

(define (swank-session-repl-thread session)
  (level-repl-thread (swank-session-level session)))

(define (swank-session-level-number session)
  (level-number (swank-session-level session)))

(define (swank-session-pusher-thread session)
  (level-pusher-thread (swank-session-level session)))

(define (swank-session-condition session)
  (level-condition (swank-session-level session)))

(define (swank-session-pending-return-tags session)
  (let loop ((level (swank-session-level session))
             (return-tags '()))
    (let ((tags (cond ((level-return-tag level)
                       => (lambda (tag) (cons tag return-tags)))
                      (else return-tags))))
      (cond ((level-parent level)
             => (lambda (parent) (loop parent tags)))
            (else tags)))))

(define (walk-swank-session-level-threads session number proc)
  (walk-level-threads (find-swank-session-level session number)
                      proc))

; (put 'walk-swank-session-level-threads 'scheme-indent-function 2)

(define (swank-session-level-thread-count session number)
  (level-thread-count (find-swank-session-level session number)))

(define (find-swank-session-level session number)
  (let loop ((level (swank-session-level session)))
    (if level
        (if (= (level-number level) number)
            level
            (loop (level-parent level)))
        (error "no such Swank level by number in session"
               session
               number))))



;;; ----------------
;;; Level scheduling

(define (spawn-swank-session-levels session top-winder top-unwinder)
  (let* ((top-dynamic-env
          (with-restarter-invoker-hook swank-restarter-invoker-hook
            (lambda ()
              (with-interaction-environment
                  (swank-world-scratch-env
                   (swank-session-world session))
                get-dynamic-env))))
         (thread
          (spawn (lambda ()
                   (dynamic-wind
                     top-winder
                     (lambda ()
                       (with-handler
                           (lambda (c punt)
                             (swank-condition-handler c punt))
                         (lambda ()
                           (set-swank-session-top-dynamic-env!
                            session
                            top-dynamic-env)
                           (push-top-level session))))
                     top-unwinder))
                 `(swank-session-scheduler
                   ,(swank-session-id session)))))
    (set-swank-session-scheduler-thread! session thread)))

(define swank-condition-handler
        (lambda (c punt)
          (punt)))

(define (push-top-level session)
  (with-level-restarters #f (swank-session-top-dynamic-env session)
    (lambda (dynamic-env)
      (reify-continuation
        (lambda (continuation)
          (let ((level (make-level #f continuation
                                   ;; No condition, return tag, or
                                   ;; pusher thread.
                                   #f #f #f
                                   dynamic-env session
                                   ;; No-op level (un)winder.
                                   values values)))
            (let-fluid $level level
              (lambda ()
                (dynamic-wind
                  (lambda ()
                    (set-swank-session-level! session level))
                  (lambda ()
                    (schedule-level level #f))
                  (lambda ()
                    ;; VALUES is a no-op level unwinder.
                    (unwind-level level values)))))))))))

(define (*push-swank-level parent return-tag condition pusher-thread
                           winder unwinder)
  (reify-continuation
    (lambda (continuation)
      (with-level-restarters parent (thread-dynamic-env pusher-thread)
        (lambda (dynamic-env)
          (let ((level (make-level parent continuation return-tag
                                   condition pusher-thread
                                   dynamic-env
                                   (level-session parent)
                                   winder unwinder)))
            (let-fluid $level level
              (lambda ()
                (dynamic-wind
                  (lambda ()
                    (set-level-child! parent level)
                    (set-swank-session-level! (level-session level)
                                              level)
                    (winder))
                  (lambda ()
                    (schedule-level level #f))
                  (lambda ()
                    (unwind-level level unwinder)))))))))))

(define $level (make-fluid #f))

(define (unwind-level level unwinder)
  (let ((unwound? #f))
    (walk-level-threads level
                        (lambda (thread)
                          (if (thread-continuation thread)
                              (terminate-level-thread thread level))))
    (dynamic-wind
      (lambda ()
        (if unwound?
            (error "trying to throw back into unwound level"
                   level)))
      (lambda ()
        (schedule-level level #t))
      (lambda ()
        (unwinder)
        (let ((parent (level-parent level)))
          (if parent (set-level-child! parent #f))
          (set-swank-session-level! (level-session level) parent))
        (set! unwound? #t)))))

;;; This ensures that THREAD is on its level's queue and terminates it.
;;; Using TERMINATE-THREAD! involves switching to the thread to run any
;;; unwind thunks, which is why it must be put on the queue first.

(define (terminate-level-thread thread level)
  (let ((queue (level-queue level)))
    (if (not (on-queue? queue thread))
        (enqueue! queue thread))
    (terminate-thread! thread)))

(define (schedule-level level unwinding?)
  (run-threads
   (round-robin-event-handler (level-queue level)
                              swank-thread-quantum
                              #f        ; Dynamic environment (set up
                                        ;   manually in SPAWN-ON-LEVEL)
                              (level-counter level)
                              (swank-event-handler level unwinding?)
                              (swank-upcall-handler level)
                              (swank-waiter level unwinding?))))

(define swank-thread-quantum 200)       ; Does this belong here?

(define (swank-event-handler level unwinding?)
  (lambda (event args)
    ;; This returns true if the event was handled and false if not.
    (destructure-enum-case event-type (cons event args)
      ((SPAWNED thread)
       (spawn-on-level thread level)
       #t)
      ((RUNNABLE thread)
       (let ((other-level (thread-level thread)))
         (if (level? other-level)
             (let loop ((level level))
               (cond ((not level)
                      (warn "dropping thread from exited level"
                            thread
                            other-level))
                     ((eq? other-level level)
                      (enqueue! (level-queue other-level) thread))
                     (else
                      (loop (level-parent level)))))
             (error "non-Swank thread running on a level"
                    thread level)))
       #t)
      ((DEADLOCK)
       (if unwinding?
           (warn "deadlock while unwinding level's threads"
                 level))
       (signal 'deadlock)
       #t)
      (else #f))))

(define (swank-waiter level unwinding?)
  (lambda ()
    ;; This returns true if the scheduler has waited and should proceed
    ;; or false if it should terminate.
    (cond ((positive? (counter-value (level-counter level)))
           (wait)                   ; Call the thread system's waiter.
           #t)
          (unwinding?
           #f)
          ((level-repl-thread level)
           ;; No threads to run, but we haven't terminated & removed
           ;; the REPL thread.
           (warn "Swank REPL has died" level '(respawning))
           (spawn-swank-repl level)
           #t)
          (else
           (let ((session (level-session level)))
             (swank-log "(session ~S) Terminating"
                        (swank-session-id session))
             ;; The controller does not run under the Swank scheduler,
             ;; so we manually terminate it here.
             (terminate-thread!
              (swank-session-controller-thread session)))
           #f))))



;;; ----------------
;;; Upcalls to the Swank scheduler

;;; Upcalls allow procedures to be called from inferior threads in the
;;; dynamic context of the Swank scheduler thread, where most of the
;;; important information, such as the current level, is stored.

(define (swank-upcall-handler level)
  (lambda (origin-thread token args)
    (if (swank-upcall? token)
        (apply (swank-upcall-procedure token)
               origin-thread level args)
        ;; Bloody spelling errors set in stone.
        (propogate-upcall origin-thread token args))))

(define-record-type* swank-upcall
  (make-swank-upcall id procedure)
  ())

(define-record-discloser :swank-upcall
  (lambda (token)
    (list 'swank-upcall (swank-upcall-id token))))

(define-syntax define-swank-upcall
  (syntax-rules ()
    ((DEFINE-SWANK-UPCALL (id arg ...) thread-var level-var
       body0 body1 ...)
     (DEFINE id
             (LET* ((PROC (LAMBDA (thread-var level-var arg ...)
                            body0 body1 ...))
                    (TOKEN (MAKE-SWANK-UPCALL 'id PROC)))
               (LAMBDA (arg ...)
                 (COND ((FLUID $LEVEL)
                        => (LAMBDA (LEVEL)
                             (PROC (CURRENT-THREAD)
                                   LEVEL
                                   arg ...)))
                       (ELSE
                        (UPCALL TOKEN arg ...)))))))))

; (put 'define-swank-upcall 'scheme-indent-function 3)

(define-swank-upcall (current-level) thread level
  level)

(define (top-level)
  (let loop ((level (current-level)))
    (cond ((level-parent level) => loop)
          (else level))))

(define-swank-upcall (current-swank-session) thread level
  (level-session level))

(define (current-swank-world)
  (swank-session-world (current-swank-session)))

(define (push-swank-level condition winder unwinder)
  (let ((thunk
         (push-swank-level/upcall (current-swank-return-tag) condition
                                  winder unwinder)))
    (thunk)))

; (put 'push-swank-level 'scheme-indent-function 1)

(define-swank-upcall (push-swank-level/upcall return-tag condition
                                              winder unwinder)
    thread level
  (*push-swank-level level return-tag condition thread
                     winder unwinder))

(define-swank-upcall (pop-swank-level) thread level
  (cond ((level-parent level)
         => resume-swank-level)
        (else
         (terminate-current-swank-session))))

(define (throw-to-level level thunk)
  (throw-to-continuation (level-continuation level)
                         thunk))

(define-swank-upcall (resume-swank-level level) thread current
  (cond ((not (eq? level current))
         (cond ((level-pusher-thread (level-child level))
                => (lambda (thread)
                     (terminate-thread! thread)
                     (if (eq? thread (level-repl-thread level))
                         (spawn-swank-repl level))))
               (else
                (warn "resuming level without a pusher thread"
                      level)))
         (throw-to-level (level-child level)
                         (lambda ()
                           (lambda ()
                             (error "returned to terminated thread"
                                    `(after resuming ,level))))))))

(define-swank-upcall (reset-swank-level level) thread current
  (throw-to-level level
                  (lambda ()
                    (if (level-parent level)
                        ;; Copy the level, essentially, throwing out
                        ;; all of the threads.
                        (*push-swank-level (level-parent level)
                                           (level-return-tag level)
                                           (level-condition level)
                                           (level-pusher-thread level)
                                           (level-winder level)
                                           (level-unwinder level))
                        (push-top-level (level-session level))))))

;++ This is a real crock.  We have to create these restarters in the
;++ dynamic environment of the level system, because we depend on the
;++ restarter invoker hook not being installed there; however, we then
;++ need to _put_ those restarters into the dynamic environment where
;++ the new level will run, so we have to manually frob the dynamic
;++ environment with GET-DYNAMIC-ENV and SET-DYNAMIC-ENV!.  (As a
;++ convenient consequence, the restarters are also available in the
;++ level scheduler thread.)

(define (with-level-restarters level dynamic-env receiver)
  (call-with-interactive-restarter 'reset
      (level-restarter-description "Reset" level)
      (lambda () (reset-swank-level (cond (level => level-child)
                                          (else (top-level)))))
      values           ; interactor
    (lambda (reset)
      (call-with-interactive-restarter 'resume
          (level-restarter-description "Resume" level)
          (lambda () (resume-swank-level (cond (level => level-child)
                                               (else (top-level)))))
          values       ; interactor
        (lambda (resume)
          (let ((saved (get-dynamic-env)))
            (set-dynamic-env! dynamic-env)
            (let ((new (with-restarter reset
                         (lambda ()
                           (with-restarter resume get-dynamic-env)))))
              (set-dynamic-env! saved)
              (receiver new))))))))

(define (level-restarter-description action level)
  (string-append action
                 (if level
                     (string-append " level "
                                    (number->string
                                     (+ 1 (level-number level))
                                     10))
                     " top level")
                 "."))

(define swank-restarter-invoker-hook
        (lambda (invoker)
          (let ((level (current-level)))
            (lambda args
              (if (eq? level (current-level))
                  (apply invoker args)
                  (throw-to-level-pusher (level-child level)
                                         (lambda ()
                                           (apply invoker args))))))))

;;; We need a separate upcall for this, because the restarter won't be
;;; invoked on the level scheduler thread, usually.

(define-swank-upcall (throw-to-level-pusher level thunk) thread current
  (throw-to-level level (lambda () thunk)))

(define-swank-upcall (terminate-current-swank-session) thread level
  (cond ((level-repl-thread level)
         => (lambda (repl-thread)
              (set-level-repl-thread! level #f)
              (terminate-thread! repl-thread)))))
