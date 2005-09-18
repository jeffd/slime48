;;; -*- mode: scheme48; package: (config) -*-

;;;;;; SLIME for Scheme48
;;;;;; Package definitions

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-structure swank-worlds swank-worlds-interface
  (open scheme
        define-record-type*
        simple-signals
        module-control
        (subset packages (make-modified-structure
                          make-simple-package))
        (subset packages-internal (set-package-integrate?!))
        (subset environments (make-reflective-tower
                              set-reflective-tower-maker!
                              environment-define!))
        weak-utilities
        )
  (optimize auto-integrate)
  (files world))

(define-structure swank-sessions swank-sessions-interface
  (open scheme
        define-record-type*
        destructure-case
        destructuring
        receiving
        srfi-2                          ;and-let*
        value-pipes
        placeholders                    ; for input requests
        locks                           ; for input request table
        tables
        cells
        weak-utilities
        simple-signals
        simple-conditions
        handle
        restarting
        restarting-hooks
        display-conditions
        (subset i/o (force-output current-error-port
                                  current-noise-port))
        string-i/o
        continuation-data-type
        fluids fluids-internal
        threads threads-internal
        scheduler
        queues
        (subset environments (interactive-environment
                              with-interaction-environment
                              set-interaction-environment!))
        swank-logging
        swank-worlds
        )
  (optimize auto-integrate)
  (files session))

(define-structure swank-i/o swank-i/o-interface
  (open scheme
        receiving
        define-record-type*
        signals
        i/o
        i/o-internal
        ports
        byte-vectors
        (subset primitives (copy-bytes!))
        proposals
        locks
        placeholders
        swank-sessions
        )
  (optimize auto-integrate)
  (files io))

(define-structure swank-tcp-servers swank-tcp-servers-interface
  (open scheme
        define-record-type*
        receiving
        sockets
        threads
        (subset threads-internal (terminate-thread!))
        placeholders
        handle
        simple-signals
        simple-conditions
        ascii
        bitwise
        (subset i/o (write-string read-block force-output))
        (subset i/o-internal (call-with-current-output-port
                              call-with-current-input-port))
        string-i/o
        swank-worlds
        swank-sessions
        swank-sldb
        swank-i/o
        swank-logging
        )
  (optimize auto-integrate)
  (files tcp-server))

(define-structure swank-logging swank-logging-interface
  (open scheme formats i/o)
  ;++ cheesy temporary implementation
  (begin (define (swank-log fmt . args)
           (format (current-noise-port) "~&[Swank: ~?]~%" fmt args))))



;;; ----------------
;;; Swank RPC implementations

(define-structure swank-general-rpc swank-general-rpc-interface
  (open scheme
        (subset posix-files (working-directory
                             set-working-directory!))
        (subset posix-process-data (get-process-id))
        (subset posix-processes (process-id->integer))
        swank-sessions
        )
  (optimize auto-integrate)
  (begin (define (swank:connection-info)
           (list (process-id->integer (get-process-id))
                 "Scheme48"             ; Lisp implementation type
                 "scheme48"             ; symbolic name for the above
                 '()                    ; empty features list
                 ':spawn                ; communication style
                 "1.3"                  ; Lisp version (argh!)
                 'nil                   ; machine instance
                 ))
         (define (swank:quit-lisp)
           ;++ should probably kill the server here
           (terminate-current-swank-session))
         (define (swank:default-directory)
           (working-directory))
         (define (swank:set-default-directory dir)
           (set-working-directory! dir)
           dir)
         ))

(define-structures ((swank-repl swank-repl-interface)
                    (swank-repl-rpc swank-repl-rpc-interface))
  (open scheme
        receiving
        formats
        string-i/o
        (subset i/o (write-string force-output
                     call-with-current-output-port))
        (subset i/o-internal (call-with-current-output-port))
        (subset display-conditions (limited-write))
        pp
        simple-signals
        packages
        (subset packages-internal (package-name))
        (subset environments (set-interaction-environment!))
        swank-sessions
        swank-worlds
        )
  (optimize auto-integrate)
  (files repl))

(define-structures ((swank-sldb swank-sldb-interface)
                    (swank-sldb-rpc swank-sldb-rpc-interface))
  (open scheme
        receiving
        srfi-2                          ;and-let*
        fluids
        simple-signals
        handle
        simple-conditions
        display-conditions
        restarting
        threads
        threads-internal
        string-i/o
        pp
        continuations
        (subset vm-exposure (primitive-catch))  ; To filter out useless
        (subset closures (closure-template))    ; continuation frames
        loopholes                               ; in backtraces.
        (subset templates (template? template-debug-data
                                     template-package-id))
        (subset disclosers (template-debug-data
                            debug-data-names))
        debug-data
        (subset names (generated? generated-name generated-uid
                                  name->symbol))
        (subset packages (structure-package package-uid))
        (subset packages-internal (package-file-name
                                   package-clauses))
        filenames                       ; for source location stuff
        ;; The next two are for constructing expressions in arbitrary
        ;; environments, maintaining invariants of Scheme forms.
        (subset meta-types (syntax-type))
        (subset nodes (get-operator))
        swank-repl                      ; for evaluating in frames
        swank-inspector                 ; for inspecting frames
        swank-worlds
        swank-sessions
        swank-logging
        )
  (optimize auto-integrate)
  (files sldb))

(define-structures ((swank-inspector swank-inspector-interface)
                    (swank-inspector-rpc
                     swank-inspector-rpc-interface))
  (open scheme
        define-record-type*
        receiving
        destructuring
        string-i/o
        simple-signals
        xvectors
        methods
        reduce                 ; looping macros
        swank-sessions
        swank-repl

        ;; for the various inspector methods
        more-types
        low-level              ; cell-unassigned & vector-unassigned?
        locations
        weak
        cells
        templates
        )
  (optimize auto-integrate)
  (files inspector))

;;; These next few stub RPC structures are necessary for any semblance
;;; of sanity in a usual SLIME interaction.  They will be replaced at
;;; some later time with real implementations, but these suffice for
;;; now.

(define-structure swank-arglist-rpc swank-arglist-rpc-interface
  (open scheme)
  (optimize auto-integrate)
  (begin (define (swank:arglist-for-echo-area names)
           'nil)
         (define (swank:variable-desc-for-echo-area name)
           'nil)
         (define (swank:arglist-for-insertion name)
           ':not-available)
         (define (swank:complete-form form-string)
           ':not-available)
         ))

(define-structure swank-completion-rpc swank-completion-rpc-interface
  (open scheme
        string-i/o
        swank-sessions
        swank-worlds
        )
  (optimize auto-integrate)
  (begin (define (swank:completions prefix package)
           (list '() prefix))
         (define (swank:simple-completions prefix package)
           (list '() prefix))
         (define (swank:fuzzy-completions prefix package)
           '())
         (define (swank:fuzzy-completion-selected orig completion)
           '())
         (define (swank:list-all-package-names include-nicknames?)
           (map write-to-string
                (swank-world-package-names (current-swank-world))))
         ))

(define-structure swank-definition-finding-rpc
    swank-definition-finding-rpc-interface
  (open scheme)
  (optimize auto-integrate)
  (begin (define (swank:find-definitions-for-emacs name)
           '())
         (define (swank:buffer-first-change filename)
           '())
         ))

;;; This macro should go somewhere else.

(define-syntax define-compound-structure
  (syntax-rules ()
    ((DEFINE-COMPOUND-STRUCTURE name component ...)
     (DEFINE-STRUCTURE name
         (COMPOUND-INTERFACE (INTERFACE-OF component)
                             ...)
       (OPEN component ...)))))

(define-compound-structure swank-rpc
  swank-general-rpc
  swank-repl-rpc
  swank-sldb-rpc
  swank-inspector-rpc
  swank-arglist-rpc
  swank-completion-rpc
  swank-definition-finding-rpc
  )



;;; ----------------
;;; Random internal utility stuff

(define-structure module-control module-control-interface
  (open scheme
        srfi-2                          ;and-let*
        simple-signals
        simple-conditions
        display-conditions
        enumerated
        (subset architecture (op exception))
        tables
        (subset i/o (silently))
        packages packages-internal
        (subset compiler-envs (environment-macro-eval))
        (subset environments (set-reflective-tower-maker!))
        bindings
        locations
        (subset meta-types (syntax-type))
        (subset names (name?))
        package-loader
        )
  (optimize auto-integrate)
  (files module))

(define-structure package-loader package-loader-interface
  (open scheme packages interfaces ensures-loaded restarting formats)
  ;++ cheesy temporary implementation -- replace with FASL library
  (begin
    (define (load-package package)
      (with-exiting-restarter 'abort
          (format #f "Abort loading of ~S." package)
        (lambda ()
          (call-with-current-continuation
            (lambda (win)
              (let lose ()
                (with-exiting-restarter 'retry
                    (format #f "Retry loading of ~S." package)
                  (lambda ()
                    (ensure-loaded
                     (make-structure package
                                     (make-simple-interface #f '())))
                    (win)))
                (lose)))))))))



;;; ----------------
;;; Utilities -- stuff that should be built-in

(define-structures ((restarting restarting-interface)
                    (restarting-hooks
                     (export with-restarter-invoker-hook)))
  (open scheme
        fluids
        receiving
        define-record-type*
        simple-signals
        ;; These two are for the RESTARTERS-IN-THREAD crock.
        (subset fluids-internal (get-dynamic-env set-dynamic-env!))
        (subset threads-internal (thread-dynamic-env))
        )
  (optimize auto-integrate)
  (files restart))

(define-structure weak-utilities weak-utilities-interface
  (open scheme
        define-record-type*
        receiving
        weak
        tables
        locks
        )
  (optimize auto-integrate)
  (files weak))

(define-structure xvectors xvectors-interface
  (open scheme define-record-type*)
  (optimize auto-integrate)
  (files xvector))

(define-structure string-i/o string-i/o-interface
  (open scheme
        extended-ports
        (subset i/o-internal (call-with-current-output-port))
        pp
        (subset display-conditions (limited-write))
        )
  (optimize auto-integrate)
  (begin (define (with-output-to-string thunk)
           (call-with-string-output-port
             (lambda (port)
               (call-with-current-output-port port
                 thunk))))
         (define (read-from-string string)
           (read (make-string-input-port string)))
         (define (print-to-string obj printer)
           (call-with-string-output-port (lambda (port)
                                           (printer obj port))))
         (define (display-to-string obj) (print-to-string obj display))
         (define (write-to-string   obj) (print-to-string obj write))
         (define (pp-to-string      obj) (print-to-string obj p))
         (define (limited-write-to-string obj depth length)
           (call-with-string-output-port
             (lambda (port)
               (limited-write obj port depth length))))
         ;; (put 'limited-write-to-string 'scheme-indent-function 1)
         ))

(define-structure destructure-case
    (export (destructure-case :syntax)
            (destructure-enum-case :syntax))
  (open scheme destructuring enum-case)
  (begin
    ;++ check input syntax more carefully
    (define-syntax destructure-case
      (lambda (form r compare)
        `(,(r 'LET) ((,(r 'LIST) ,(cadr form)))
           (,(r 'CASE) (,(r 'CAR) ,(r 'LIST))
             ,@(map (lambda (clause)
                      (if (compare (car clause) (r 'ELSE))
                          `(,(r 'ELSE) ,@(cdr clause))
                          `(,(if (list? (caar clause))
                                 (caar clause)
                                 (list (caar clause)))
                            (,(r 'DESTRUCTURE)
                                ((,(cdar clause)
                                  (,(r 'CDR) ,(r 'LIST))))
                              ,@(cdr clause)))))
                    (cddr form)))))
      (CAR CDR LET CASE ELSE DESTRUCTURE))

    (define-syntax destructure-enum-case
      (lambda (form r compare)
        `(,(r 'LET) ((,(r 'LIST) ,(caddr form)))
           (,(r 'ENUM-CASE) ,(cadr form) (,(r 'CAR) ,(r 'LIST))
             ,@(map (lambda (clause)
                      (if (compare (car clause) (r 'ELSE))
                          `(,(r 'ELSE) ,@(cdr clause))
                          `(,(if (list? (caar clause))
                                 (caar clause)
                                 (list (caar clause)))
                            (,(r 'DESTRUCTURE)
                                ((,(cdar clause)
                                  (,(r 'CDR) ,(r 'LIST))))
                              ,@(cdr clause)))))
                    (cdddr form)))))
      (CAR CDR LET ENUM-CASE ELSE DESTRUCTURE))

    ; (put 'destructure-case 'scheme-indent-function 1)
    ; (put 'destructure-enum-case 'scheme-indent-function 2)
    ))

(define-structure continuation-data-type
    continuation-data-type-interface
  (open scheme define-record-types)
  (optimize auto-integrate)
  (files continuation))
