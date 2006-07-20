;;; -*- mode: scheme; scheme48-package: (config) -*-

;;;;;; SLIME for Scheme48
;;;;;; Interface definitions

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-interface swank-worlds-interface
  (export make-swank-world
          swank-world?
          swank-world-id
          swank-world-sessions
          next-swank-session-id
          swank-world-scratch-env
          swank-world-config-env
          swank-world-rpc-env
          swank-world-structure-names
          swank-world-package-names
          find-structure-in-swank-world
          find-package-in-swank-world
          make-swank-envs
          ))

(define-interface swank-sessions-interface
  (export spawn-swank-session
          swank-session?
          swank-session-world
          swank-session-id

          ;; Sending & receiving messages to & from the session's
          ;; controller.
          send-incoming-swank-message
          send-outgoing-swank-message      ; for RPC implementations
          abort-swank-rpc                  ; ditto
          receive-outgoing-swank-message   ; for Swank/Emacs interface

          interrupt-swank-thread

          request-swank-input           ; random

          make-swank-session-slot
          (define-swank-session-slot :syntax)

          ;; Level-specific data
          swank-session-level-number
          swank-session-repl-thread
          swank-session-pusher-thread
          swank-session-condition
          swank-session-pending-return-tags
          walk-swank-session-level-threads
          swank-session-level-thread-count
          swank-thread-for-id

          current-swank-session
          current-swank-world
          current-swank-return-tag

          push-swank-level
          pop-swank-level
          terminate-current-swank-session
          ))

(define-interface swank-i/o-interface
  (export make-swank-output-port
          make-swank-input-port
          make-swank-tcp-output-port
          ))

(define-interface swank-tcp-servers-interface
  (export
    make-one-shot-swank-tcp-server
    spawn-swank-tcp-server
    close-swank-tcp-server
    swank-tcp-server?
    swank-tcp-server-world
    swank-tcp-server-port-number
    encode-swank-message
    decode-swank-message
    ))

(define-interface swank-logging-interface
  (export swank-log
          ))



;;; ----------------
;;; Swank RPC interfaces

;;; Exports marked with (*) are not intended to be implemented, for
;;; whatever reason.

(define-interface swank-general-rpc-interface
  (export swank:connection-info
          swank:quit-lisp
          swank:default-directory
          swank:set-default-directory
          ))

(define-interface swank-repl-interface
  (export repl-eval-string
          interactive-eval-results
          pprint-eval-results
          delimited-object-list-string  ; This should go elsewhere.
          swank-repl-presentations?
          enable-swank-repl-presentations
          disable-swank-repl-presentations
          toggle-swank-repl-presentations
          ))

(define-interface swank-repl-rpc-interface
  (export swank:interactive-eval
          swank:eval-and-grab-output
          swank:repl-eval-hook-pass           ;(*)
          swank:repl-suppress-output          ;(*)
          swank:repl-suppress-advance-history ;(*)
          swank:interactive-eval-region
          swank:re-evaluate-defvar            ;(*)
          swank:pprint-eval
          swank:set-package
          swank:use-package
          swank:undefine-function
          swank:listener-eval
          swank:get-repl-result
          swank:clear-repl-results
          ))

(define-interface swank-sldb-interface
  (export with-sldb-handler
          sldb-condition-handler
          ))

(define-interface swank-sldb-rpc-interface
  (export swank:simple-break
          swank:sldb-break-with-default-debugger
          swank:backtrace
          swank:debugger-info-for-emacs
          swank:invoke-nth-restart-for-emacs
          swank:invoke-nth-restart      ;(*)
          swank:sldb-abort
          swank:sldb-continue
          swank:throw-to-toplevel
          swank:eval-string-in-frame
          swank:pprint-eval-string-in-frame
          swank:frame-locals-for-emacs
          swank:frame-catch-tags-for-emacs
          swank:frame-source-location-for-emacs
          swank:sldb-disassemble
          swank:sldb-return-from-frame
          swank:restart-frame
          swank:sldb-break-on-return
          swank:sldb-step               ;(*)
          swank:inspect-in-frame
          swank:inspect-current-condition
          swank:inspect-frame-var
          swank:sldb-break
          ))

(define-interface swank-inspector-interface
  (export inspect-results
          ;** Note!  There is a huge difference between these two
          ;** exports.  The first initiates the Swank inspector; the
          ;** second is a method table for an internal operation in the
          ;** inspector that produces a title, type symbol, and list of
          ;** components for a given object in order to inspect it.
          inspect-object
          &inspect-object
          ))

(define-interface swank-inspector-rpc-interface
  (export swank:init-inspector
          swank:inspector-reinspect
          swank:inspect-nth-part
          swank:inspector-nth-part
          swank:inspector-call-nth-action ;(*)
          swank:pprint-inspector-part
          swank:inspector-pop
          swank:inspector-next
          swank:quit-inspector
          swank:describe-inspectee        ;(*)
          ))

(define-interface swank-arglist-rpc-interface
  (export swank:arglist-for-echo-area
          :print-right-margin           ;++ This is a real crock.
          :print-lines
          :arg-indices
          swank:variable-desc-for-echo-area
          swank:arglist-for-insertion
          swank:complete-form
          ))

(define-interface swank-completion-rpc-interface
  (export swank:completions
          swank:simple-completions
          swank:fuzzy-completions
          swank:fuzzy-completion-selected
          swank:list-all-package-names
          ))

(define-interface swank-apropos-rpc-interface
  (export swank:apropos-list-for-emacs
          ))

(define-interface swank-definition-finding-rpc-interface
  (export swank:find-definitions-for-emacs
          swank:buffer-first-change
          ))

(define-interface swank-profiling-rpc-interface
  (export swank:toggle-profile-fdefinition
          swank:profile-package
          swank:profiled-functions
          swank:profile-report
          swank:profile-reset
          swank:unprofile-all
          ))

(define-interface swank-tracing-rpc-interface
  (export swank:swank-toggle-trace
          swank:untrace-all
          ))

(define-interface swank-compiler/loader-rpc-interface
  (export swank:load-file
          swank:load-file-set-package
          swank:compile-file-for-emacs
          swank:compile-file-if-needed
          swank:compile-string-for-emacs
          swank:compiler-notes-for-emacs
          swank:operate-on-system-for-emacs
          swank:list-all-systems-in-central-registry
          ))

(define-interface swank-macro-expansion-rpc-interface
  (export swank:swank-macroexpand-1
          swank:swank-macroexpand
          swank:swank-macroexpand-all
          swank:compiler-macroexpand
          swank:compiler-macroexpand-1
          ))

(define-interface swank-disassembly-rpc-interface
  (export swank:disassemble-symbol
          ))

(define-interface swank-description-rpc-interface
  (export swank:describe-symbol
          swank:describe-function
          swank:describe-definition-for-emacs
          swank:documentation-symbol    ;(*)
          ))

(define-interface swank-xref-rpc-interface
  (export swank:xref
          ))

(define-interface swank-thread-control-rpc-interface
  (export swank:list-threads
          swank:quit-thread-browser
          swank:debug-nth-thread
          swank:kill-nth-thread
          swank:start-swank-server-in-thread ;(*)
          ))

(define-interface swank-indentation-rpc-interface
  (export swank:update-indentation-information
          ))



;;; ----------------
;;; Internal facilities that might be useful elsewhere

(define-interface module-control-interface
  (export make-config-package
          config-find-structure
          config-find-package
          fold-config-structures
          config-structure-names
          config-package-names
          maybe-environment-ref
          maybe-structure-ref
          package-reflective-tower
          uid->package
          package-open!
          package-undefine!
          loaded-interaction-environment
          semi-loaded-interaction-environment
          ))

(define-interface package-loader-interface
  (export load-package
          ))

(define-interface debugger-utilities-interface
  (export continuation-frame-list
          continuation-frame-ref
          ignorable-frame?
          with-ignorable-frame-template
          continuation-debug-data
          frame-preview
          display-debug-data-names
          display-frame-source
          destructure-source-info
          eval-in-frame
          frame-locals-list
          template-source-location
          ))



(define-interface restarting-interface
  (export make-restarter
          restarter?
          restarter-tag
          restarter-description
          restart
          restart-interactively
          current-restarters
          restarters-in-thread
          with-restarter
          find-restarter
          call-with-restarter
          call-with-interactive-restarter
          with-exiting-restarter
          call-with-exiting-restarter
          ))

(define-interface weak-utilities-interface
  (export make-weak-set
          weak-set?
          weak-set-empty?
          weak-set-add!
          weak-set-delete!
          weak-set-pop!
          weak-set-fold
          weak-set-fold*
          weak-set-walk
          weak-set-find
          weak-set-contains?
          weak-set->list

          make-weak-table
          weak-table-ref
          weak-table-set!
          weak-table-walk
          ))

(define-interface xvectors-interface
  (export make-xvector
          xvector-length
          xvector-ref
          xvector-push!
          xvector-maybe-push!
          xvector-index
          ))

(define-interface string-i/o-interface
  (export make-string-input-port
          make-string-output-port
          string-output-port-output
          call-with-string-output-port
          with-output-to-string
          read-from-string
          display-to-string
          write-to-string
          pp-to-string
          extended-write-to-string
          shared-write-to-string
          limited-write-to-string
          hybrid-write-to-string
          ))

(define-interface extended-writing-interface
  (export
    extended-write
    limited-write
    shared-write
    hybrid-write
    $write-depth
    $write-breadth
    write-depth
    write-breadth
    set-write-depth!
    set-write-breadth!
    with-writing-limits
    $write-limited?
    $write-shared?
    write-limited?
    write-shared?
    set-write-limited?!
    set-write-shared?!
    with-limited-writing
    with-shared-writing
    with-hybrid-writing
    with-preserved-writing
    ))

(define-interface continuation-data-type-interface
  (export reify-continuation
          continuation?
          throw-to-continuation
          return-from-continuation
          with-continuation))
