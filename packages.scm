;;; -*- mode: scheme; scheme48-package: (config) -*-

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
        cells
        proposals
        )
  (optimize auto-integrate)
  (files world))

(define-structure swank-sessions swank-sessions-interface
  (open scheme
        define-record-type*
        destructure-case
        destructuring
        receiving
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
        formats
        string-i/o
        (subset i/o (force-output))
        continuation-data-type
        fluids fluids-internal
        threads threads-internal
        scheduler
        queues
        enumerated
        (subset architecture (interrupt))
        (subset environments (with-interaction-environment
                              set-interaction-environment!))
        packages
        packages-internal
        (subset package-mutation (package-system-sentinel))
        swank-logging
        swank-worlds
        )
  (optimize auto-integrate)
  (files session))

(define-structure swank-i/o swank-i/o-interface
  (open scheme
        receiving
        define-record-type*
        simple-signals
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
        receiving
        sockets
        threads
        (subset threads-internal (terminate-thread!))
        handle
        simple-signals
        simple-conditions
        ascii
        bitwise
        (subset i/o (write-string read-block force-output))
        (subset string-i/o (read-from-string write-to-string))
        swank-sessions
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
        fluids
        (subset posix-files (working-directory
                             set-working-directory!))
        (subset sockets (get-host-name))
        swank-sessions
        swank-versions
        )
  (optimize auto-integrate)
  (files general))

(define-structures ((swank-repl swank-repl-interface)
                    (swank-repl-rpc swank-repl-rpc-interface))
  (open scheme
        receiving
        string-i/o
        i/o
        (subset i/o-internal (call-with-current-output-port))
        extended-writing
        pp
        simple-signals
        weak-utilities
        reduce
        package-loader
        packages
        (subset packages-internal (package-name))
        (subset environments (set-interaction-environment!))
        module-control
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
        (subset display-conditions (display-condition))
        restarting
        threads
        threads-internal
        string-i/o
        extended-writing
        pp
        continuations
        debugger-utilities
        disassembler     ;++ flush (see comment by SWANK-INSPECTOR)
        ;; The next two are for accessing PUSH-SWANK-LEVEL's template
        ;; to filter it out of backtraces.
        (subset closures (closure-template))
        loopholes
        (subset names (generated? generated-name generated-uid
                                  name->symbol))
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
        extended-writing
        (subset i/o (write-string))
        simple-signals
        xvectors
        methods
        module-control
        reduce                 ; looping macros
        swank-sessions
        swank-repl

        ;; for the various inspector methods
        more-types
        low-level              ; cell-unassigned & vector-unassigned?
        locations
        (subset packages-internal (package-name-table))
        weak
        cells
        templates
                               ;++ This brings in a dependency on the
        disassembler           ;++ Scheme48 command processor and ought
                               ;++ to be replaced.
        byte-vectors
        closures
        debug-data
        disclosers
        records
        record-types
        handle
        tables
        ports
        architecture
        bitwise
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
  (begin (define (swank:arglist-for-echo-area names . options)
           'nil)
         (define :print-right-margin ':print-right-margin)
         (define :print-lines ':print-lines)
         (define :arg-indices ':arg-indices)
         (define (swank:variable-desc-for-echo-area name)
           'nil)
         (define (swank:arglist-for-insertion name)
           ':not-available)
         (define (swank:complete-form form-string)
           ':not-available)
         ))

(define-structure swank-completion-rpc swank-completion-rpc-interface
  (open scheme
        receiving
        handle
        (subset util (fold))
        (subset string-i/o (write-to-string read-from-string))
        (subset packages-internal (for-each-definition
                                   for-each-export
                                   package-opens))
        swank-sessions
        swank-worlds
        )
  (optimize auto-integrate)
  (files completion))

(define-structure swank-apropos-rpc swank-apropos-rpc-interface
  (open scheme
        sort
        destructuring
        string-i/o
        simple-signals
        (subset packages (structure? structure-package package-uid))
        (subset packages-internal (for-each-definition
                                   for-each-export
                                   package-opens
                                   structure-name))
        meta-types
        bindings
        locations
        (subset transforms (transform?))
        (subset nodes (operator?))
        (subset primops (primop?))
        (subset names (name->symbol))
        posix-regexps
        (subset swank-sessions (current-swank-world
                                abort-swank-rpc))
        swank-worlds
        )
  (optimize auto-integrate)
  (files apropos))

(define-structure swank-definition-finding-rpc
    swank-definition-finding-rpc-interface
  (open scheme
        module-control
        string-i/o
        debugger-utilities
        (subset display-conditions (display-condition))
        closures
        destructuring
        (subset disclosers (template-debug-data))
        (subset debug-data (debug-data-name)))
  (optimize auto-integrate)
  (begin (define (swank:find-definitions-for-emacs name)
           (maybe-environment-ref (interaction-environment)
               (read-from-string name)
             (lambda (value)
               (if (closure? value)
                   (cond ((template-source-location
                           (closure-template value)
                           ;; PC is non-#F only for continuations.
                           #f)
                          => (lambda (location)
                               ;; One location -> one-element list.
                               `((,(hybrid-write-to-string
                                    (debug-data-name
                                     (template-debug-data
                                      (closure-template value))))
                                  ,location))))
                         (else 'nil))
                   'nil))
             (lambda (condition)
               `((,name (:ERROR ,(call-with-string-output-port
                                   (lambda (port)
                                     (display-condition condition
                                                        port)))))))))

         (define (swank:buffer-first-change filename)
           '())
         ))

(define-structure swank-compiler/loader-rpc
    swank-compiler/loader-rpc-interface
  (open scheme
        receiving
        string-i/o
        filenames
        module-control
        package-loader
        (subset packages (package->environment
                          structure-package
                          package-uid
                          link!))
        (subset compiler-envs (bind-source-file-name))
        syntactic
        (subset compiler (compile-forms))
        compile-packages       ;++ replace for FASL library
        (subset closures (make-closure))
        (subset vm-exposure (invoke-closure))
        handle
        simple-conditions
        (subset display-conditions (display-condition))
        restarting
        time
        (subset swank-repl-rpc (swank:set-package))
        swank-sessions
        swank-worlds
        )
  (optimize auto-integrate)
  (files loader))

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
  swank-apropos-rpc
  swank-definition-finding-rpc
  swank-compiler/loader-rpc
  )



;;; ----------------
;;; Random internal utility stuff

(define-structure module-control module-control-interface
  (open scheme
        srfi-2                          ;and-let*
        simple-signals
        (subset simple-conditions (make-vm-exception))
        enumerated
        (subset architecture (op exception))
        (subset primitives (find-all-records))
        (subset i/o (silently))
        packages packages-internal
        (subset compiler-envs (environment-macro-eval))
        (subset environments (set-reflective-tower-maker!))
        bindings
        locations
        (subset meta-types (syntax-type))
        (subset names (name?))
        package-mutation
        package-loader
        ensures-loaded                  ;++ flush
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

(define-structure debugger-utilities debugger-utilities-interface
  (open scheme
        receiving
        srfi-2
        fluids
        simple-signals
        simple-conditions
        handle
        continuations
        (subset vm-exposure (primitive-catch))
        (subset templates (template? template-package-id))
        (subset disclosers (template-debug-data debug-data-names))
        debug-data
        string-i/o
        extended-writing
        ;; The next two are for constructing expressions in arbitrary
        ;; environments, maintaining invariants of Scheme forms.
        (subset nodes (get-operator))
        (subset meta-types (syntax-type))
        module-control
        (subset packages-internal (package-file-name package-clauses))
        filenames
        )
  (optimize auto-integrate)
  (files debug-util))



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
        extended-writing
        )
  (optimize auto-integrate)
  (begin
    (define (with-output-to-string thunk)
      (call-with-string-output-port
        (lambda (port)
          (call-with-current-output-port port
            thunk))))
    (define (read-from-string string)
      (read (make-string-input-port string)))
    (define (to-string obj outputter)
      (call-with-string-output-port
        (lambda (port)
          (outputter obj port))))
    (define (display-to-string          obj) (to-string obj display))
    (define (write-to-string            obj) (to-string obj write))
    (define (pp-to-string               obj) (to-string obj p))
    (define (extended-write-to-string   obj) (to-string obj extended-write))
    (define (limited-write-to-string    obj) (to-string obj limited-write))
    (define (shared-write-to-string     obj) (to-string obj shared-write))
    (define (hybrid-write-to-string     obj) (to-string obj hybrid-write))
    ))

(define-structure extended-writing extended-writing-interface
  (open scheme
        fluids
        cells
        writing
        simple-signals
        (subset i/o (make-null-output-port write-string))
        )
  (optimize auto-integrate)
  (files write))

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
