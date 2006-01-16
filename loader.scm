;;; -*- mode: scheme; scheme48-package: swank-compiler/loader-rpc -*-

;;;;;; SLIME for Scheme48
;;;;;; Swank loader RPC implementations

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (swank:load-file filename)
  (load
   ;++ This is a crock.
   (if (let ((len (string-length filename)))
         (and (> len 4)
              (not (string=? (substring filename (- len 4) len)
                             ".scm"))))
       (string-append filename ".scm")
       filename)
   (interaction-environment))
  'nil)

(define (swank:load-file-set-package filename package-string)
  (load filename (interaction-environment))
  (swank:set-package package-string))

(define (swank:compile-file-for-emacs filename load? . encoding)
  ;++ respect encoding
  (with-swank-compiler
    (lambda ()
      (let* ((package (interaction-environment))
             (template (swank-compile-file filename package)))
        (if load?
            (swank-run-template template (package-uid package))))
      't)))

;;; Files are not compiled to be fasdumped, so this operation isn't
;;; really meaningful in Scheme48.

(define (swank:compile-file-if-needed filename load?)
  (swank:compile-file-for-emacs filename load?))

(define (swank:compile-string-for-emacs string buffer pos dir)
  (let ((filename (and buffer (namestring buffer dir #f))))
    (with-swank-compiler
      (lambda ()
        (let* ((package (interaction-environment))
               (cenv (let ((cenv (package->environment package)))
                       (if filename
                           (bind-source-file-name filename cenv)
                           cenv)))
               (template (swank-compile-forms
                          (read-all (make-string-input-port string))
                          package
                          cenv
                          (and buffer (namestring buffer dir #f)))))
          (swank-run-template template
                              (package-uid package)))
        '()))))



;;; 'System' operations (module commands)

(define (swank:operate-on-system-for-emacs system operation . args)
  (let ((operate-on-package
         (lambda (operator)
           (cond ((find-structure-in-swank-world
                   (read-from-string system)
                   (current-swank-world))
                  => (lambda (struct)
                       (with-swank-compiler
                         (lambda ()
                           (operator (structure-package struct))
                           'nil))))
                 (else
                  (abort-swank-rpc))))))
    (case (read-from-string operation)
      ((load-op)
       (operate-on-package load-package))
      ((compile-op)
       (operate-on-package compile-package))
      (else
       (abort-swank-rpc)))))

(define (swank:list-all-systems-in-central-registry)
  (map symbol->string
       (swank-world-structure-names (current-swank-world))))



;;; Compiler notes

(define-swank-session-slot swank-compiler-notes
  set-swank-compiler-notes!
  modify-swank-compiler-notes!
  '())

(define (swank:compiler-notes-for-emacs)
  (swank-compiler-notes))

(define (clear-swank-compiler-notes)
  (set-swank-compiler-notes! '()))

(define (record-swank-compiler-note note)
  (modify-swank-compiler-notes!
   (lambda (notes) (cons note notes))))

(define (with-swank-compiler continuation)
  (clear-swank-compiler-notes)
  (call-with-current-continuation
    (lambda (exit)
      (call-with-interactive-restarter 'abort "Abort compilation."
          (lambda () (abort-swank-rpc)) ; invoker
          (lambda () (values))          ; interactor
        (lambda (abort)
          (receive (result milliseconds)
                   (measure-time-interval
                    (lambda ()
                      (with-handler
                          (lambda (c punt)
                            (cond ((condition->compiler-note c)
                                   => record-swank-compiler-note))
                            (punt))
                        continuation)))
            (list (limited-write-to-string result)
                  (milliseconds->time-string milliseconds))))))))

(define (condition->compiler-note condition)
  (let ((generate-stuff
         (lambda ()
           `(:MESSAGE ,(call-with-string-output-port
                         (lambda (port)
                           (display-condition condition port)))
             :SHORT-MESSAGE
               ,@(cond ((condition-short-message condition) => list)
                       (else '()))
             :LOCATION (:ERROR "No source location for condition.")
             :REFERENCES ()))))
    (cond ((error? condition)
           `(:SEVERITY ,(if (read-error? condition)
                            ':READ-ERROR
                            ':ERROR)
             ,@(generate-stuff)))
          ((warning? condition)
           `(:SEVERITY :WARNING ,@(generate-stuff)))
          ((note? condition)
           `(:SEVERITY :NOTE ,@(generate-stuff)))
          (else #f))))

(define (condition-short-message condition)
  (and (pair? condition)
       (pair? (cdr condition))
       (string? (cadr condition))
       (cadr condition)))



;;; Compiler interface

(define (swank-compile-file filename package)
  (swank-compile-forms (call-with-input-file filename
                         read-all)
                       package
                       (bind-source-file-name
                        filename
                        (package->environment package))
                       filename))

(define (swank-compile-forms forms package cenv context)
  (let* ((uid (package-uid package))
         (template
          (compile-forms (map (lambda (form)
                                (delay (expand-scanned-form form cenv)))
                              (scan-forms forms cenv))
                         context
                         (package-uid package))))
    (link! template package #t)         ; #T -> note undefined refs
    template))

(define (swank-run-template template package-key)
  (invoke-closure (make-closure template package-key)))



;;; Miscellaneous utilities

(define (read-all port)
  (let loop ((items '()))
    (let ((item (read port)))
      (if (eof-object? item)
          (reverse items)
          (loop (cons item items))))))

(define (measure-time-interval procedure)
  (let* ((start (real-time))
         (result (procedure))
         (end (real-time)))
    (values result (- end start))))

(define (milliseconds->time-string milliseconds)
  (let* ((n (quotient milliseconds 10))
         (q (quotient n 100))
         (r (remainder n 100)))
    (string-append (number->string q 10)
                   "."
                   (if (< r 10) "0" "")
                   (number->string r 10))))
