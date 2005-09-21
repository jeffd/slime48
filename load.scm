;;; -*- mode: scheme; scheme48-package: (exec) -*-

;;;;;; SLIME for Scheme48
;;;;;; Load script

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(config '(load "=slime48/defrectype*.scm"
               "=slime48/interfaces.scm"
               "=slime48/packages.scm"))

;;; Get rid of Scheme48's crap of a SRFI 35 implementation.

(new-package)
(open 'exceptions-internal 'vm-exceptions)
(run '(begin (initialize-vm-exceptions! really-signal-condition)
             #t)) ; Don't print what INITIALIZE-VM-EXCEPTIONS! returns.
(user)

;;; Fix BREAKPOINT.

(in 'debugging '(run (define (breakpoint . args)
                       (command-loop (cons 'breakpoint args)))))

;;; Load the Swank back end, and create a top-level wrapper in a new
;;; package.

(load-package 'swank-rpc)

(config '(structure swank-structures (export scheme
                                             module-system
                                             built-in-structures
                                             swank-rpc)))
(in 'package-commands-internal
    '(structure config-package (export config-package)))

(config '(run (define-structure slime48
                  (export slime48
                          make-slime48-world
                          spawn-slime48-tcp-server
                          )
                (open scheme
                      receiving
                      srfi-2            ;and-let*
                      (subset i/o (force-output))
                      (subset i/o-internal
                              (call-with-current-output-port
                               call-with-current-input-port))
                      string-i/o
                      restarting
                      handle
                      simple-conditions
                      simple-signals
                      vm-exceptions
                      (subset architecture (enum op exception))
                      locations
                      templates
                      (subset packages (structure-package
                                        package-lookup))
                      (subset bindings (binding-place))
                      (subset disclosers (location-name))
                      swank-structures
                      config-package
                      swank-worlds
                      swank-i/o
                      swank-tcp-servers
                      swank-sldb
                      )
                (optimize auto-integrate)
                ;; Not sure whether the =slime48/ is necessary.
                (files =slime48/top))))

(define (slime48-start . port-opt)
  (in 'slime48
      `(RUN (BEGIN (DEFINE SLIME48-WORLD)
                   (DEFINE SLIME48-TCP-SERVER)
                   (CALL-WITH-VALUES
                       ,(if (pair? port-opt)
                            `(LAMBDA () (SLIME48 ',(car port-opt)))
                            'SLIME48)
                     (LAMBDA (WORLD SERVER)
                       (SET! SLIME48-WORLD WORLD)
                       (SET! SLIME48-TCP-SERVER SERVER)))))))
