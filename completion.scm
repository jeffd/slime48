;;; -*- mode: scheme; scheme48-package: swank-completion-rpc -*-

;;;;;; SLIME for Scheme48
;;;;;; Name completion

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (swank:list-all-package-names include-nicknames?)
  (map write-to-string
       ;; This is a bit of a hack.  Scheme48 has a distinction between
       ;; structures and packages, which CL doesn't; some commands want
       ;; packages (to enter, which is invalid for stable packages),
       ;; and some commands want structures (to open, which is valid
       ;; for structures with any packages, stable or unstable), and
       ;; there is no way to make CL SLIME differentiate between the
       ;; two.  (Actually, it might make sense for it to distinguish
       ;; between locked & unlocked packages in CL, which could serve
       ;; as the same distinction as that between structures & packages
       ;; here.)  This is the most useful behaviour I can come up with.
       (swank-world-structure-names (current-swank-world))))

(define (swank:fuzzy-completions prefix-string package-spec)
  '())

(define (swank:fuzzy-completion-selected orig-string completion)
  '())

(define (swank:completions prefix-string package-spec)
  ;++ implement compound completions
  (swank:simple-completions prefix-string package-spec))

(define (swank:simple-completions prefix-string package-spec)
  (let ((world (current-swank-world))
        (package-id (if (string? package-spec)
                        (read-from-string package-spec)
                        package-spec)))
    (cond ((find-package-in-swank-world package-id world)
           => (lambda (package)
                (let ((completions
                       (compute-completions prefix-string package
                                            string-prefix?)))
                  (list completions
                        (longest-common-prefix completions)))))
          (else
           (abort-swank-rpc
            "(world ~S, COMPLETION) No such package by name: ~A"
            (swank-world-id world)
            package-spec)))))

(define (compute-completions prefix-string package completion?)
  (let ((completions '())
        (prefix-string
         ;++ hack
         (symbol->string (read-from-string prefix-string))))
    (define (test symbol)
      (if (symbol? symbol)     ; protect against generated names
          (let ((string (symbol->string symbol)))
            (if (completion? prefix-string string)
                (set! completions (cons string completions))))))
    (for-each-definition (lambda (symbol binding)
                           (test symbol))
                         package)
    (for-each (lambda (open)
                (for-each-export (lambda (symbol type binding)
                                   (test symbol))
                                 open))
              (package-opens package))
    completions))

(define (string-prefix? prefix string)
  (let ((prefix-len (string-length prefix)))
    (and (<= prefix-len (string-length string))
         (let loop ((i 0))
           (cond ((= i prefix-len) #t)
                 ((char=? (string-ref prefix i)
                          (string-ref string i))
                  (loop (+ i 1)))
                 (else #f))))))

(define (longest-common-prefix strings)
  (if (null? strings)
      ""
      (fold (lambda (s1 s2)
              (receive (len shorter)
                       (let ((s1-len (string-length s1))
                             (s2-len (string-length s2)))
                         (if (< s1-len s2-len)
                             (values s1-len s1)
                             (values s2-len s2)))
                (let loop ((i 0))
                  (cond ((= i len)
                         shorter)
                        ((char=? (string-ref s1 i)
                                 (string-ref s2 i))
                         (loop (+ i 1)))
                        (else
                         (substring shorter 0 i))))))
            (cdr strings)
            (car strings))))
