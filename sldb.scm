;;; -*- mode: scheme48; package: swank-sldb -*-

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
                            (swank-user-interrupt? condition))
                        (sldb-condition-handler condition punt)
                        (punt))))
    thunk))

; (put 'with-sldb-handler 'scheme-indent-function 1)

;;; This really ought to be SWANK:BREAK; that it can be CL:BREAK is an
;;; artifact of the orientation in SLIME toward Common Lisp.

(define (cl:break)
  (with-exiting-restarter 'continue "Continue from break."
    (lambda ()
      (sldb-condition-handler '(breakpoint)
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
             ,(continuation-frame-list (sldb-continuation)
                                       0 20) ; same as in CL Swank
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

(define (sldb-continuation)
  (thread-continuation (sldb-thread-to-debug)))

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
                    (sldb-limited-write-to-string condition)))
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
  (let loop ((cont (sldb-continuation))
             (i 0))
    (cond ((not cont) '())
          ((= i start)
           (continuation-frame-list cont start (- end start)))
          (else
           (loop (continuation-cont cont)
                 (if (ignorable-frame? cont)
                     i
                     (+ i 1)))))))

(define (swank:debugger-info-for-emacs start end)
  (list (sldb-disclose-condition)
        (sldb-restarter-listing)
        (swank:backtrace start end)
        (sldb-pending-return-tags)))

(define (swank:invoke-nth-restart-for-emacs level n)
  (if (= level (swank-session-level-number (current-swank-session)))
      (let loop ((restarters (sldb-restarters))
                 (n n))
        (cond ((null? restarters)
               (swank-log "(session ~S, level ~S) Restart index ~S ~A"
                          (swank-session-id (current-swank-session))
                          n
                          "out of bounds")
               (abort-swank-rpc))
              ((zero? n)
               (restart-interactively (car restarters)))
              (else
               (loop (cdr restarters)
                     (- n 1)))))
      ;; Silently ignore the request if it's on the wrong level.
      'nil))

;;; If we're running in a Swank RPC, there will always be an ABORT
;;; restarter, because it's installed as the RPC aborter before calling
;;; the actual procedure.  However, we must filter out the first ABORT
;;; restarter, because it corresponds with the SLDB-ABORT RPC, which is
;;; rather useless to invoke.

(define (swank:sldb-abort)
  (let loop ((rs (sldb-restarters)) (flag #f))
    (cond ((null? rs)
           (error "no ABORT restarter (internal inconsistency)"
                  '(SWANK:SLDB-ABORT)
                  (sldb-restarters)))
          ((eq? (restarter-tag (car rs))
                'abort)
           (if flag
               (restart (car rs))
               (loop (cdr rs) #t)))
          (else
           (loop (cdr rs) flag)))))

;;; The same is not true, however, of CONTINUE restarters.

(define (swank:sldb-continue)
  (cond ((find-restarter 'continue)
         => (lambda (r) (restart r)))
        (else
         (let ((session (current-swank-session)))
           (swank-log "(session ~S, level ~S) No CONTINUE restarter"
                      (swank-session-id session)
                      (swank-session-level-number session)))
         (abort-swank-rpc))))

;;; This is very different from what the CL Swank back end does.  I
;;; don't understand how that works, though.  This just picks out the
;;; last RESET restarter, which should be the one to the top level.

(define (swank:throw-to-toplevel)
  (let loop ((rs (sldb-restarters)) (reset #f))
    (cond ((null? rs)
           (if reset
               (restart reset)
               (swank:sldb-abort)))
          ((eq? (restarter-tag (car rs))
                'reset)
           (loop (cdr rs) (car rs)))
          (else
           (loop (cdr rs) reset)))))

(define (swank:inspect-current-condition)
  (inspect-object (sldb-condition)))

(define (swank:inspect-frame-var frame-number var-number)
  (inspect-object
   (continuation-arg
    (continuation-frame-ref (sldb-continuation) frame-number)
    var-number)))

(define (swank:inspect-in-frame string n)
  (call-with-values (lambda () (eval-in-frame n string))
    inspect-results))

(define (swank:eval-string-in-frame string n)
  (eval-in-frame* n string
    (lambda () "; No values")
    (lambda (v)
      (sldb-limited-write-to-string v))
    (lambda (vals)
      (delimited-object-list-string vals sldb-limited-write ","))))

(define (swank:pprint-eval-string-in-frame string n)
  (eval-in-frame* n string
    (lambda () "; No values")
    (lambda (v) (pp-to-string v))
    (lambda (vals)
      (delimited-object-list-string vals p ""))))

(define (eval-in-frame* n string zero one many)
  (receive results (eval-in-frame n string)
    (cond ((null? results) (zero))
          ((null? (cdr results)) (one (car results)))
          (else (many results)))))

(define (eval-in-frame n string)
  (call-with-sldb-frame n
    (lambda () (repl-eval-string string))
    (lambda (frame ddata pc)
      (*eval-in-frame string frame ddata pc))))

; (put 'eval-in-frame 'scheme-indent-function 2)

(define (*eval-in-frame string frame ddata pc)
  (let ((package (guess-frame-package ddata
                                      (template-package-id
                                       (continuation-template frame))))
        (bindings (filter (lambda (x) (and (car x) #t))
                          (frame-locals-list frame ddata
                                             make-local-binding)))
        (exp (read-from-string string)))
    (eval (if (pair? bindings)
              `((,operator/lambda ,(map car bindings)
                  ,exp)
                ,@(map cdr bindings))
              exp)
          (or package (interaction-environment)))))

(define (make-local-binding name value)
  (cons name `(,operator/code-quote ,value)))

(define operator/lambda (get-operator 'lambda syntax-type))
(define operator/code-quote (get-operator 'code-quote syntax-type))

(define (guess-frame-package ddata uid)
  (and-let* ((names (debug-data-names ddata))
             ((pair? names)))
    (let loop ((names names))
      (let ((more (cdr names)))
        (if (pair? more)
            (loop more)
            (and-let* ((package (find-package-in-swank-world
                                 (car names)
                                 (current-swank-world)))
                       ;; Sanity check.
                       ((eq? uid (package-uid package))))
              package))))))

;;; No such thing as a catch tag in Scheme.

(define (swank:frame-catch-tags-for-emacs n) '())

(define (swank:frame-locals-for-emacs n)
  (call-with-sldb-frame n
    (lambda () '())
    (lambda (frame ddata pc)
      (frame-locals-list frame ddata make-frame-local-for-emacs))))

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
                  (sldb-limited-write-to-string name)))
    :ID ,(if (generated? name)
             (generated-uid name)
             0)
    :VALUE ,(sldb-limited-write-to-string value)))

(define (frame-locals-list frame ddata make-frame-local)
  (*frame-locals-list (let ((arg-count (continuation-arg-count frame)))
                        (do ((i 0 (+ i 1))
                             (args '()
                                   (cons (continuation-arg frame i)
                                         args)))
                            ((= i arg-count) args)))
                      (debug-data-env-shape
                       ddata
                       (continuation-pc frame))
                      0
                      '()
                      make-frame-local))

(define (*frame-locals-list args shape i tail make-frame-local)
  (if (null? args)
      tail
      (let ((probe (assv i shape)))
        (if (and probe (not (null? (cdr probe))))
            (named-frame-locals-list (cdr probe) args shape i tail
                                     make-frame-local)
            (*frame-locals-list (cdr args) shape
                                (+ i 1)
                                (cons (make-frame-local #f (car args))
                                      tail)
                                make-frame-local)))))

(define (named-frame-locals-list names args shape i tail
                                 make-frame-local)
  (cond ((null? names)
         (*frame-locals-list args shape i tail make-frame-local))
        ((pair? (car names))            ; environment
         (let ((env-vector (car args)))
           (do ((ns (car names) (cdr ns))
                (j 0 (+ j 1))
                (tail tail (cons (make-frame-local
                                  (car ns)
                                  (vector-ref env-vector j))
                                 tail)))
               ((null? ns)
                (named-frame-locals-list (cdr names)
                                         (cdr args) shape
                                         (+ i 1)
                                         tail
                                         make-frame-local)))))
        (else
         (named-frame-locals-list (cdr names)
                                  (cdr args) shape
                                  (+ i 1)
                                  (cons (make-frame-local (car names)
                                                          (car args))
                                        tail)
                                  make-frame-local))))

(define (swank:frame-source-location-for-emacs n)
  (let ((lose (lambda ()
                `(:ERROR ,(string-append
                           "No source location for frame "
                           (number->string n 10))))))
    (call-with-sldb-frame n
      lose
      (lambda (frame ddata pc)
        (or (debug-data-source-location ddata pc)
            (lose))))))

(define (call-with-sldb-frame n lose win)
  (let ((frame (continuation-frame-ref (sldb-continuation) n)))
    (if (continuation? frame)
        (call-with-continuation-debug-data frame
          lose
          (lambda (ddata pc)
            (win frame ddata pc)))
        (lose))))

; (put 'call-with-sldb-frame 'scheme-indent-function 1)



;;; Finding source locations

;++ This should go somewhere else.

(define (debug-data-source-location ddata pc)
  (let ((names (debug-data-names ddata)))
    (and (pair? names)
         (let ((hints (source-location-hints ddata pc)))
           (if (pair? (cdr names))
               (let loop ((names names))
                 (if (pair? (cddr names))
                     (loop (cdr names))
                     (find-source-location (cadr names) (car names)
                                           hints)))
               (find-source-location (car names) #f hints))))))

(define (source-location-hints ddata pc)
  (cond ((assv pc (debug-data-source ddata))
         => (lambda (source-info)
              (receive (exp index parent cwv?)
                       (destructure-source-info source-info)
                `(:SNIPPET ,(sldb-limited-write-to-string exp)
                  ,@(if parent
                        `(:CALL-SITE ,(symbol->string (car parent)))
                        '())))))
        (else '())))

(define (find-source-location top def hints)
  (let ((def (if def
                 `(:FUNCTION-NAME ,(symbol->string def))
                 'NIL)))
    (if (string? top)
        `(:LOCATION (:FILE ,top) ,def ,hints)
        (and-let* ((struct (find-structure-in-swank-world
                            top
                            (current-swank-world)))
                   (package (structure-package struct))
                   (filename (package-file-name package))
                   (base (file-name-directory filename))
                   (source-filename
                    (any (lambda (clause)
                           (and (eq? (car clause) 'FILES)
                                (pair? (cdr clause))
                                (source-file-exists? (cadr clause)
                                                     base)))
                         (package-clauses package))))
          `(:LOCATION (:FILE ,source-filename)
                      ,def
                      ,hints)))))

(define (any fn list)
  (and (pair? list)
       (or (fn (car list))
           (any fn (cdr list)))))

;++ This is a hideous kludge.

(define (source-file-exists? name base)
  (call-with-current-continuation
    (lambda (k)
      (with-handler (lambda (c punt) (if (error? c) (k #f) (punt)))
        (lambda ()
          (let ((name (translate
                       (namestring name base *scheme-file-type*))))
            (close-input-port (open-input-file name))
            name))))))



;;; Generating frame listings for backtraces &c.

(define (continuation-frame-list cont start count)
  (let loop ((cont cont)
             (i 0) (number start)
             (frames '()))
    (cond ((or (= i count)
               (not cont))
           (reverse frames))
          ((ignorable-frame? cont)
           (loop (continuation-cont cont) i number frames))
          (else
           (loop (continuation-cont cont)
                 (+ i 1) (+ number 1)
                 (cons (list number (frame-preview cont))
                       frames))))))

(define (continuation-frame-ref cont n)
  (let loop ((cont cont) (i 0))
    (cond ((not cont)
           #f)
          ((ignorable-frame? cont)
           (loop (continuation-cont cont) i))
          ((= i n)
           cont)
          (else
           (loop (continuation-cont cont) (+ i 1))))))

;;; Filter out continuations within LET-FLUID(S), DYNAMIC-WIND, and
;;; PUSH-SWANK-LEVEL; they are not useful to the user.

(define ignorable-frame?
  (let ((fluid-template
         (let-fluid (make-fluid #f) #f
           (lambda ()
             (primitive-catch
               (lambda (cont)
                 (or (continuation-template cont)
                     (begin (warn
                        "unable to filter LET-FLUID out of backtraces")
                            #f)))))))
        (wind-template
         (dynamic-wind
           values
           (lambda ()
             (primitive-catch
               (lambda (cont)
                 (or (continuation-template cont)
                     (begin (warn
                     "unable to filter DYNAMIC-WIND out of backtraces")
                            #f)))))
           values))
        (swank-level-pusher-template
         (closure-template (loophole :closure push-swank-level))))
    (lambda (frame)
      (cond ((continuation-template frame)
             => (lambda (template)
                  (or (eq? template fluid-template)
                      (eq? template wind-template)
                      (eq? template swank-level-pusher-template))))
            (else #t)))))

(define (call-with-continuation-debug-data cont lose win)
  (let ((template (continuation-template cont)))
    (if (not (template? template))
        (lose)
        (let ((ddata (template-debug-data template)))
          (if (debug-data? ddata)
              (win ddata (continuation-pc cont))
              (lose))))))

; (put 'call-with-continuation-debug-data 'scheme-indent-function 1)

(define (frame-preview cont)
  (call-with-continuation-debug-data cont
    (lambda () "<no frame information>")
    (lambda (ddata pc)
      (call-with-string-output-port
        (lambda (port)
          (display-frame-source ddata pc port)
          (newline port)
          (display "      In: " port)
          (let ((names (debug-data-names ddata)))
            (if (let loop ((names names))
                  (or (null? names)
                      (and (not  (car names))
                           (loop (cdr names)))))
                (write `(anonymous ,(if (debug-data? ddata)
                                        (debug-data-uid ddata)
                                        ddata))
                       port)
                (begin (write (or (car names) '(anonymous))
                              port)
                       (for-each (lambda (name)
                                   (display " <- " port)
                                   (write (or name '(anonymous))
                                          port))
                                 (cdr names))))))))))

(define (display-frame-source ddata pc port)
  (cond ((assv pc (debug-data-source ddata))
         => (lambda (source-info)
              (receive (exp index parent cwv?)
                       (destructure-source-info source-info)
                (sldb-limited-write exp port)
                (cond ((and index parent)
                       (newline port)
                       (display "      Waiting for: " port)
                       (sldb-limited-write
                        (append (take index parent)
                                (if cwv?
                                    '((LAMBDA () ^^^))
                                    '(^^^))
                                (drop (+ index 1)
                                      parent))
                        port))))))
        (else
         (display "<no frame source information>" port))))

(define (destructure-source-info source)
  ;; SOURCE is either a list (<exp>) or (<exp> <index> . <parent>),
  ;; where <exp> is the expression of a continuation, <index> is the
  ;; index of <exp> as a subexpression of <parent>, and <parent> is the
  ;; combination of which <exp> is a subexpression that needed a
  ;; continuation to evaluate.
  (let ((exp (cadr source)))
    (if (and (pair? (cddr source))
             (integer? (caddr source))
             (list? (cdddr source)))
        (let ((exp (cadr source))
              (index (caddr source))
              (parent (cdddr source)))
          ;; CALL-WITH-VALUES continuations have the expression wrapped
          ;; in a LAMBDA, so you might see a debugger frame that would
          ;; look like this, if we didn't frobnicate those frames:
          ;;   4: (lambda () (foo bar baz))
          ;;       Waiting for: (call-with-values ^^^ #)
          ;; Instead, because we frobnicate, it looks like this:
          ;;   4: (foo bar baz)
          ;;       Waiting for: (call-with-values (lambda () ^^^) #)
          (if (and (eq? (car parent) 'CALL-WITH-VALUES)
                   (eqv? index 1)
                   (eq? (car exp) 'LAMBDA))
              (values (caddr exp) index parent #t)
              (values exp index parent #f)))
        (values exp #f #f #f))))

(define (take n list)
  (let recur ((list list) (i 0))
    (if (= i n)
        '()
        (cons (car list)
              (recur (cdr list) (+ i 1))))))

(define (drop n list)
  (let loop ((list list) (i 0))
    (if (= i n)
        list
        (loop (cdr list)
              (+ i 1)))))



;;; Random

(define (sldb-limited-write obj port)
  (limited-write obj port (sldb-print-depth) (sldb-print-length)))

(define (sldb-limited-write-to-string obj)
  (limited-write-to-string obj
    (sldb-print-depth)
    (sldb-print-length)))

(define (sldb-print-depth)  4)
(define (sldb-print-length) 5)

(define (filter pred list)              ; reversing filter
  (let loop ((in list) (out '()))
    (if (null? in)
        out
        (loop (cdr in)
              (if (pred (car in))
                  (cons (car in) out)
                  out)))))
