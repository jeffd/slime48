;;; -*- mode: scheme; scheme48-package: debugger-utilities -*-

;;;;;; SLIME for Scheme48
;;;;;; Utilities for implementing debuggers

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

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
;;; whatever else the debugger doesn't want (e.g., PUSH-SWANK-LEVEL);
;;; they are not useful to the user.

(define (ignorable-frame? frame)
  (cond ((continuation-template frame)
         => (lambda (template)
              (and (memq template (fluid $ignorable-frame-templates))
                   #t)))
        (else #f)))

(define $ignorable-frame-templates
        (make-fluid
         (let ((get-template
                (lambda (name)
                  (primitive-catch
                    (lambda (frame)
                      (cond ((continuation-template frame)
                             => list)
                            (else
                             (warn (string-append
                                    "unable to filter "
                                    (string-upcase
                                     (symbol->string name))
                                    " out of backtraces"))
                             '())))))))
           (append (let-fluid (make-fluid #f) #f
                     (lambda ()
                       (get-template 'let-fluid)))
                   (dynamic-wind
                     values
                     (lambda ()
                       (get-template 'dynamic-wind))
                     values)))))

(define (string-upcase string)
  (let* ((len (string-length string))
         (new (make-string len)))
    (do ((i 0 (+ i 1)))
        ((= i len) new)
      (string-set! new i (char-upcase (string-ref string i))))))

(define (with-ignorable-frame-template template thunk)
  (let-fluid $ignorable-frame-templates
      (cons template (fluid $ignorable-frame-templates))
    thunk))

(define (continuation-debug-data cont)
  (let ((template (continuation-template cont)))
    (if (template? template)
        (let ((ddata (template-debug-data template)))
          (if (debug-data? ddata)
              ddata
              #f))
        #f)))

; (put 'call-with-continuation-debug-data 'scheme-indent-function 1)

(define (frame-preview cont)
  (cond ((continuation-debug-data cont)
         => (lambda (ddata)
              (call-with-string-output-port
                (lambda (port)
                  (display-frame-source ddata
                                        (continuation-pc cont)
                                        port)
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
                                           (write (or name
                                                      '(anonymous))
                                                  port))
                                         (cdr names)))))))))
        (else "<no frame information>")))

(define (display-frame-source ddata pc port)
  (cond ((assv pc (debug-data-source ddata))
         => (lambda (source-info)
              (receive (exp index parent cwv?)
                       (destructure-source-info source-info)
                (limited-write exp port)
                (cond ((and index parent)
                       (newline port)
                       (display "      Waiting for: " port)
                       (limited-write
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
                   (pair? exp)
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



;;; Evaluation in frames

(define (eval-in-frame exp frame)
  (let ((bindings (filter (lambda (x) (and (car x) #t))
                          (frame-locals-list frame
                                             make-local-binding))))
    (eval (if (pair? bindings)
              `((,operator/lambda ,(map car bindings)
                  ,exp)
                ,@(map cdr bindings))
              exp)
          (or (uid->package (template-package-id
                             (continuation-template frame)))
              (interaction-environment)))))

(define (make-local-binding name value)
  (cons name `(,operator/code-quote ,value)))

(define operator/lambda (get-operator 'lambda syntax-type))
(define operator/code-quote (get-operator 'code-quote syntax-type))



;;; Listing named frame local variables

(define (frame-locals-list frame make-frame-local)
  (cond ((continuation-debug-data frame)
         => (lambda (ddata)
              (*frame-locals-list
               (let ((arg-count (continuation-arg-count frame)))
                 (do ((i 0 (+ i 1))
                      (args '()
                            (cons (continuation-arg frame i)
                                  args)))
                     ((= i arg-count) args)))
               (debug-data-env-shape ddata (continuation-pc frame))
               0
               '()
               make-frame-local)))
        (else #f)))

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



;;; Finding source locations

(define (template-source-location template pc)
  (cond ((template-debug-data template)
         => (lambda (ddata)
              (debug-data-source-location
               ddata pc
               (template-package-id template))))
        (else #f)))

(define (debug-data-source-location ddata pc pkg-uid)
  (let ((names (debug-data-names ddata)))
    (and (pair? names)
         (let ((hints (source-location-hints ddata pc)))
           (if (pair? (cdr names))
               (let loop ((names names))
                 (if (pair? (cddr names))
                     (loop (cdr names))
                     (find-source-location (cadr names) (car names)
                                           hints
                                           pkg-uid)))
               (find-source-location (car names) #f hints
                                     pkg-uid))))))

(define (source-location-hints ddata pc)
  (cond ((assv pc (debug-data-source ddata))
         => (lambda (source-info)
              (receive (exp index parent cwv?)
                       (destructure-source-info source-info)
                `(:SNIPPET ,(limited-write-to-string exp)
                  ,@(if parent
                        `(:CALL-SITE ,(symbol->string (car parent)))
                        '())))))
        (else '())))

(define (find-source-location top def hints pkg-uid)
  (let ((def (if def
                 `(:FUNCTION-NAME ,(symbol->string def))
                 'NIL)))
    (if (string? top)
        `(:LOCATION (:FILE ,top) ,def ,hints)
        (and-let* ((package (uid->package pkg-uid))
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



;;; Random

(define (filter pred list)              ; reversing filter
  (let loop ((in list) (out '()))
    (if (null? in)
        out
        (loop (cdr in)
              (if (pred (car in))
                  (cons (car in) out)
                  out)))))
