;;; -*- mode: scheme; scheme48-package: swank-apropos-rpc -*-

;;;;;; SLIME for Scheme48
;;;;;; Apropos

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; This differs slightly from the CL APROPOS facility because there is
;;; not one well-defined notion of 'exported symbol' in Scheme48, since
;;; multiple structures can be built on one package.  Instead, we use
;;; the boolean flag to decide whether to search all bindings exported
;;; by every structure in the config package, or to search all bindings
;;; available in a certain package.

(define (swank:apropos-list-for-emacs string all? case-sensitive?
                                      package-spec)
  (let ((matches? (apropos-matcher string case-sensitive?))
        (world (current-swank-world)))
    (cond ((or all?
               (eq? package-spec 'nil))
           (apropos-all (swank-world-config-env world) matches?))
          ((find-package-in-swank-world
            (if (string? package-spec)
                (read-from-string package-spec)
                package-spec)
            world)
           => (lambda (package)
                (apropos-package package matches?)))
          (else (abort-swank-rpc)))))

(define (apropos-matcher string case-sensitive?)
  (cond ((zero? (string-length string))
         (lambda (string) #t))
        (else
         (let ((regexp (if case-sensitive?
                           (make-regexp string)
                           (make-regexp string
                                        (regexp-option ignore-case)))))
           (lambda (string)
             ;; No submatches, starts line, ends line.
             (regexp-match regexp string 0 #f #t #t))))))

(define (apropos-all config-env matches?)
  (let* ((results '())
         (push (lambda (elt) (set! results (cons elt results))))
         (body (lambda (id binding)
                 (if (binding? binding)
                     (let ((loc (binding-place binding)))
                       (if (and (location? loc)
                                (location-assigned? loc)
                                (structure? (contents loc)))
                           (let ((struct (contents loc)))
                             (for-each-export
                              (lambda (id type binding)
                                (*apropos id type binding
                                          "exported by" struct
                                          matches? push))
                              struct))))))))
    (for-each-definition body config-env)
    (for-each (lambda (struct)
                (for-each-export (lambda (id type binding)
                                   (body id binding))
                                 struct))
              (package-opens config-env))
    (apropos-postprocess results)))

(define (apropos-package package matches?)
  (let* ((results '())
         (push (lambda (elt) (set! results (cons elt results)))))
    (for-each-definition (lambda (id binding)
                           (*apropos id #f binding #f #f
                                     matches? push))
                         package)
    (for-each (lambda (struct)
                (for-each-export (lambda (id type binding)
                                   (*apropos id type binding
                                             "imported from" struct
                                             matches? push))
                                 struct))
              (package-opens package))
    (apropos-postprocess results)))

(define (*apropos id exported-type binding msg struct matches? push)
  (if (and (binding? binding)
           (matches? (symbol->string id)))
      (push (list id exported-type binding msg struct))))

(define (apropos-postprocess results)
  (let loop ((results (sort-list! results apropos-result<?))
             (out '()))
    (if (null? results)
        (reverse out)
        (loop (cdr results)
              (cons (postprocess-result (car results))
                    out)))))

;;; Sort by these random criteria:
;;; - Local bindings go first.
;;; - Bindings exported by the same structure go together.
;;; - If structures' names are available, sort structures' binding
;;;   groups by those.
;;; - If not, sort by structure uid.
;;; - Sort by name of binding last.

(define (apropos-result<? a b)
  (destructure (((id-a type-a binding-a msg-a struct-a) a)
                ((id-b type-b binding-b msg-b struct-b) b))
    (let ((id<? (lambda ()
                  (string<? (symbol->string id-a)
                            (symbol->string id-b)))))
      (if struct-a
          (if struct-b
              (let ((suid-a (package-uid (structure-package struct-a)))
                    (sname-a (structure-name struct-a))
                    (suid-b (package-uid (structure-package struct-b)))
                    (sname-b (structure-name struct-b)))
                (cond ((= suid-a suid-b)
                       (id<?))
                      ((and sname-a sname-b)
                       (let ((sstring-a (symbol->string sname-a))
                             (sstring-b (symbol->string sname-b)))
                         (if (string=? sstring-a sstring-b)
                             (id<?)
                             (string<? sstring-a sstring-b))))
                      (else
                       (< suid-a suid-b))))
              #f)
          (if struct-b #t (id<?))))))

(define (append-reverse list tail)
  (if (null? list)
      tail
      (append-reverse (cdr list)
                      (cons (car list) tail))))

(define (postprocess-result result)
  (destructure (((id exported-type binding msg struct) result))
    `(:designator
      ,(if (and msg struct)
           (string-append (write-to-string id)
                          " (" msg " " (write-to-string struct)
                          ")")
           (write-to-string id))
      ,@(let ((type (or exported-type (binding-type binding))))
          (cond ((eq? type syntax-type)
                 (syntactic-binding id binding))
                ((subtype? type value-type)
                 (variable-binding id binding type))
                (else
                 (warn "funny binding" id binding)
                 '()))))))

(define (syntactic-binding id binding)
  (let* ((static (binding-static binding))
         (win (lambda (type) `(,type ,(write-to-string static)))))
    (cond ((transform? static) (win ':macro))
          ((operator?  static) (win ':special-operator))
          (else (warn "funny syntactic binding"
                      id binding)
                '()))))

(define (variable-binding id binding type)
  `(,@(if (eq? type value-type)
          '()
          `(:type ,(write-to-string (type->sexp type #t))))
    :variable ,(let ((loc (binding-place binding)))
                 (if (location? loc)
                     (cond ((location-assigned? loc)
                            (limited-write-to-string (contents loc)
                              (apropos-print-depth)
                              (apropos-print-length)))
                           ((location-defined? loc)
                            "[unassigned]")
                           (else
                            "[undefined]"))
                     "[funny binding]"))))

(define (apropos-print-depth) 2)
(define (apropos-print-length) 3)
