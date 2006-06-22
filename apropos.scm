;;; -*- mode: scheme; scheme48-package: swank-apropos-rpc -*-

;;;;;; SLIME for Scheme48
;;;;;; Apropos

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; This differs slightly from the CL APROPOS facility because there is
;;; not one well-defined notion of 'exported symbol' in Scheme48, since
;;; multiple structures can be built on one package.  Instead, we use
;;; the boolean flag to decide whether to search through bindings
;;; exported by structures available in the config package or whether
;;; to examine the bindings visible from within a particular package.

(define (swank:apropos-list-for-emacs string exported? case-sensitive?
                                      where)
  (let ((matches? (apropos-matcher string case-sensitive?))
        (world (current-swank-world)))
    (let ((where (if (string? where) (read-from-string where) where)))
      (if exported?
          (cond ((eq? where 'nil)
                 (apropos-all (swank-world-config-env world) matches?))
                ((find-structure-in-swank-world where world)
                 => (lambda (struct)
                      (apropos-structure struct matches?)))
                (else
                 (abort-swank-rpc
                  "(world ~S, APROPOS) No such structure by name: ~A"
                  (swank-world-id world)
                  where)))
          (cond ((eq? where 'nil)
                 (apropos-package (interaction-environment) matches?))
                ((find-package-in-swank-world where world)
                 => (lambda (package)
                      (apropos-package package matches?)))
                (else
                 (abort-swank-rpc
                  "(world ~S, APROPOS) No such package by name: ~A"
                  (swank-world-id world)
                  where)))))))

(define (apropos-matcher string case-sensitive?)
  (cond ((zero? (string-length string))
         (lambda (symbol) symbol #t))
        (else
         (let ((regexp (if case-sensitive?
                           (make-regexp string)
                           (make-regexp string
                                        (regexp-option ignore-case)))))
           (lambda (symbol)
             (regexp-match regexp
                           (symbol->string symbol)
                           ;; No submatches, starts line, ends line.
                           0 #f #t #t))))))

(define (apropos-all config-env matches?)
  (call-with-apropos matches?
    (lambda (record)
      (define (body id binding)
        (if (binding? binding)
            (let ((loc (binding-place binding)))
              (if (and (location? loc)
                       (location-assigned? loc)
                       (structure? (contents loc)))
                  (*apropos-structure (contents loc)
                                      "exported by"
                                      record)))))
      (for-each-definition body config-env)
      (for-each (lambda (struct)
                  (for-each-export (lambda (id type binding)
                                     type
                                     (body id binding))
                                   struct))
                (package-opens config-env)))))

(define (apropos-structure structure matches?)
  (call-with-apropos matches?
    (lambda (record)
      (*apropos-structure structure "exported by" record))))

(define (*apropos-structure struct message record)
  (for-each-export (lambda (id type binding)
                     (record id type binding message struct))
                   struct))

(define (apropos-package package matches?)
  (call-with-apropos matches?
    (lambda (record)
      (for-each-definition (lambda (id binding)
                             (record id #f binding #f #f))
                           package)
      (for-each (lambda (struct)
                  (*apropos-structure struct "imported from" record))
                (package-opens package)))))

(define (call-with-apropos matches? collector)
  (let ((results '()))
    (define (apropos-record id exported-type binding message struct)
      (let ((symbol (name->symbol id)))
        (if (and (binding? binding)
                 (matches? symbol))
            (set! results
                  (cons (list id exported-type binding message struct)
                        results)))))
    (collector apropos-record)
    (apropos-postprocess results)))

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

(define (postprocess-result result)
  (destructure (((id exported-type binding msg struct) result))
    `(:DESIGNATOR
      ,(if (and msg struct)
           (string-append (write-to-string id)
                          " (" msg " " (write-to-string struct)
                          ")")
           (write-to-string id))
      ,@(let ((type (or exported-type (binding-type binding))))
          (cond ((or (eq? type undeclared-type)
                     (variable-type? type)
                     (subtype? type value-type))
                 (variable-binding id binding type))
                ((subtype? type syntax-type)
                 (syntactic-binding id binding))
                (else
                 (warn "funny binding" id binding)
                 '()))))))

(define (syntactic-binding id binding)
  (let* ((static (binding-static binding))
         (win (lambda (type) `(,type ,(write-to-string static)))))
    (cond ((transform? static) (win ':MACRO))
          ((operator?  static) (win ':SPECIAL-OPERATOR))
          (else (warn "funny syntactic binding"
                      id binding)
                '()))))

(define (variable-binding id binding type)
  `(,@(if (eq? type value-type)
          '()
          `(:TYPE ,(write-to-string (type->sexp type #t))))
    :VARIABLE ,(string-append
                (let ((static (binding-static binding)))
                  (cond ((primop?    static) "(primitive procedure) ")
                        ((transform? static) "(integrable procedure) ")
                        (else "")))
                (let ((loc (binding-place binding)))
                  (if (location? loc)
                      (cond ((location-assigned? loc)
                             (limited-write-to-string (contents loc)))
                            ((location-defined? loc)
                             "[unassigned]")
                            (else
                             "[undefined]"))
                      "[funny binding]")))))
