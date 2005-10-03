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

          interrupt-swank-thread        ;++ Why export this?
          swank-user-interrupt?
          swank-user-interrupt-thread
          swank-user-interrupt-args

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
  (export spawn-swank-tcp-server
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
          repl-eval-string*
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
          swank:listener-eval
          swank:get-repl-result
          swank:clear-repl-results
          ))

(define-interface swank-sldb-interface
  (export with-sldb-handler
          sldb-condition-handler
          ))

(define-interface swank-sldb-rpc-interface
  (export cl:break                      ; eck
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
          swank:call-nth-action         ;(*)
          swank:inspector-pop
          swank:inspector-next
          swank:quit-inspector
          swank:describe-inspectee
          ))

(define-interface swank-arglist-rpc-interface
  (export swank:arglist-for-echo-area
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

(define-interface swank-definition-finding-rpc-interface
  (export swank:find-definitions-for-emacs
          swank:buffer-first-change
          ))



;;; ----------------
;;; Internal facilities that might be useful elsewherea

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
          package-open!
          package-undefine!
          ))

(define-interface package-loader-interface
  (export load-package
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
          circular-write-to-string
          limited-write-to-string
          ))

(define-interface continuation-data-type-interface
  (export reify-continuation
          continuation?
          throw-to-continuation
          return-from-continuation
          with-continuation))
