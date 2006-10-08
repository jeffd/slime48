;;; -*- mode: scheme; scheme48-package: swank-inspector -*-

;;;;;; SLIME for Scheme48
;;;;;; Inspector

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-swank-session-slot current-inspector
  set-current-inspector!
  modify-current-inspector!
  #f)

(define-record-type* inspector
  (make-inspector (object))
  ((parts (make-xvector))
   (stack '())
   (history (let ((history (make-xvector)))
              (xvector-push! history object)
              history))
   (position 0)))

(define (current-inspector-object)
  (inspector-object (current-inspector)))
(define (set-current-inspector-object! obj)
  (set-inspector-object! (current-inspector) obj))

(define (current-inspector-parts)
  (inspector-parts (current-inspector)))
(define (set-current-inspector-parts! parts)
  (set-inspector-parts! (current-inspector) parts))

(define (current-inspector-stack)
  (inspector-stack (current-inspector)))
(define (set-current-inspector-stack! stack)
  (set-inspector-stack! (current-inspector) stack))
(define (modify-current-inspector-stack! modifier)
  (let ((inspector (current-inspector)))
    (set-inspector-stack! inspector
                          (modifier (inspector-stack inspector)))))

(define (current-inspector-history)
  (inspector-history (current-inspector)))

(define (current-inspector-position)
  (inspector-position (current-inspector)))
(define (set-current-inspector-position! position)
  (set-inspector-position! (current-inspector) position))

(define-record-type* zero-values
  (zero-values)
  ())

(define (one-value x) x)

(define-record-type* multiple-values
  (multiple-values list)
  ())

(define (swank:init-inspector exp-string . reset?-option)
  (cond ((repl-eval-string exp-string)
         => (lambda (results)
              (inspect-results results
                               (and (pair? reset?-option)
                                    (car reset?-option)))))
        (else
         (abort-swank-rpc
          "(session ~S, INSPECT) No expression in string: ~S"
          (swank-session-id (current-swank-session))
          exp-string))))

(define (swank:inspector-nth-part n)
  (xvector-ref (current-inspector-parts) n))

(define (swank:inspect-nth-part n)
  (inspect-subobject (swank:inspector-nth-part n)))

(define (swank:pprint-inspector-part n)
  (pp-to-string (swank:inspector-nth-part n)))

(define (swank:inspector-pop)
  (let ((stack (current-inspector-stack)))
    (cond ((pair? stack)
           (set-current-inspector-stack! (cdr stack))
           (let ((obj (car stack)))
             (set-current-inspector-object! obj)
             (set-current-inspector-position!
              (xvector-index (current-inspector-history)
                             obj)))
           (swank:inspector-reinspect))
          (else 'nil))))

(define (swank:inspector-next)
  (let ((history (current-inspector-history))
        (position (+ 1 (current-inspector-position))))
    (if (= position (xvector-length history))
        'nil
        (inspect-subobject (xvector-ref history position)))))

(define (swank:quit-inspector)
  (set-current-inspector! #f)
  'nil)

(define (inspect-object obj)
  (set-current-inspector! (make-inspector obj))
  (swank:inspector-reinspect))

(define (inspect-results results . reset?-option)
  ((if (or (and (pair? reset?-option)
                (car reset?-option))
           (not (current-inspector)))
       inspect-object
       inspect-subobject)
   (cond ((null? results)
          (zero-values))
         ((null? (cdr results))
          (one-value (car results)))
         (else
          (multiple-values results)))))

(define (inspect-subobject obj)
  (modify-current-inspector-stack!
   (lambda (stack)
     (cons (current-inspector-object) stack)))
  (set-current-inspector-position!
   (xvector-maybe-push! (current-inspector-history) obj))
  (set-current-inspector-object! obj)
  (swank:inspector-reinspect))

(define (swank:inspector-reinspect)
  (receive (title type listing)
           (really-inspect-object (current-inspector-object))
    (let ((contents (process-inspector-listing listing)))
      `(:TITLE   ,(call-with-string-output-port
                    (lambda (output-port)
                      (write-string title output-port)
                      (newline output-port)
                      (newline output-port)
                      (hybrid-write (current-inspector-object)
                                    output-port)))
        :TYPE    ,(string-upcase (symbol->string type))
        :CONTENT ,contents
        :ID      ,(xvector-push! (inspector-parts (current-inspector))
                                 (current-inspector-object))))))

(define (process-inspector-listing listing)
  (let ((parts (current-inspector-parts)))
    (reduce ((list* item listing))
        ((contents '()))
      ((lambda (content)
         (cons content contents))
       (cond ((string?     item) item)
             ((char?       item) (string item))
             ((eq? newline item) (string #\newline))
             ((symbol?     item) (symbol-label  item))
             ((integer?    item) (integer-label item))
             ((pair?       item)
              (destructure (( (obj . printed) item))
                `(:VALUE ,(if (string? printed)
                              printed
                              (hybrid-write-to-string obj))
                         ,(xvector-push! parts obj))))
             (else
              (error "invalid inspection listing item"
                     item))))

      (reverse contents))))

(define (symbol-label symbol)
  (string-append (string-upcase (symbol->string symbol))
                 ": "))

(define (integer-label integer)
  (string-append "  "
                 (number->string integer 10)
                 ": "))

(define-generic really-inspect-object &inspect-object (object))

(define-method &inspect-object (obj)
  (cond ((disclose obj)
         => (lambda (type.components)
              (values "An object."
                      (car type.components)
                      (append-map (lambda (x)
                                    `((,x) ,newline))
                                  (cdr type.components)))))
        (else
         (values "An indeterminate object."
                 'object
                 ;; In the title is limited output; show the *whole*
                 ;; output here, but be sure to terminate on circular
                 ;; structures.
                 ;++ What we really want is a pretty-printer that can
                 ;++ show shared structure as well, but we don't have
                 ;++ that at the moment.
                 `(,(shared-write-to-string obj))))))

(define-method &inspect-object ((obj :zero-values))
  (values "Zero return values."
          'zero-values
          '()))

(define-method &inspect-object ((obj :multiple-values))
  (let ((vals (multiple-values-list obj)))
    (values (string-append (number->string (length vals) 10)
                           " return values.")
            'multiple-values
            (reduce ((list* val vals)
                     (count* i 0))
                ((contents '()))
              (append-reverse `(,i (,val) ,newline) contents)

              (reverse contents)))))



;;; Miscellaneous data

(define-method &inspect-object ((symbol :symbol))
  (values "A symbol." 'symbol
          `("String: " (,(symbol->string symbol)) ,newline)))

(define-method &inspect-object ((string :string))
  (values "A string." 'string '()))

(define-method &inspect-object ((char :char))
  (values "A character." 'char '()))

(define-method &inspect-object ((boolean :boolean))
  (values "A boolean." 'boolean '()))

(define-method &inspect-object ((obj :eof-object))
  (values "The end of file object." 'eof-object '()))

(define-method &inspect-object ((loc :location))
  (values "A location (top-level variable cell)."
          'location
          (receive (name package-uid)
                   (cond ((location-info loc)
                          => (lambda (info)
                               (values (car info) (cdr info))))
                         (else (values #f #f)))
            `("ID: " (,(location-id loc)) ,newline
              "Name: " (,name) ,newline
              "Package: " (,(or (uid->package package-uid)
                                (table-ref package-name-table
                                           package-uid)
                                package-uid))
              ,newline ,newline
              ,(cond ((not (location-defined? loc)) "{undefined}")
                     ((not (location-assigned? loc)) "{unassigned}")
                     (else `(,(contents loc))))
              ,newline))))

(define-method &inspect-object ((cell :cell))
  (values "A cell."
          'cell
          `("Value: " ,(if (cell-unassigned? cell)
                           "{unassigned}"
                           (list (cell-ref cell)))
            ,newline)))

(define-method &inspect-object ((weak :weak-pointer))
  (values "A weak pointer."
          'weak-pointer
          `("Ref: " (,(weak-pointer-ref weak)) ,newline)))



;;; Numbers

(define (number-in-radices number)
  `("Binary: #b" ,(number->string number 2) ,newline
    "Octal: #o" ,(number->string number 8) ,newline
    "Decimal: #d" ,(number->string number 10) ,newline
    "Hexadecimal: #x" ,(number->string number 16) ,newline))

(define-method &inspect-object ((number :number))
  (values "A number."
          'number
          (number-in-radices number)))

(define-method &inspect-object ((complex :complex))
  (values "A complex number."
          'complex
          `("Real part: " (,(real-part complex)) ,newline
            "Imaginary part: " (,(imag-part complex)) ,newline)))

(define-method &inspect-object ((real :real))
  (values "A real number."
          'real
          ;++ This should show the underlying raw flonum bit string.
          (number-in-radices real)))

(define-method &inspect-object ((rational :rational))
  (values "A rational number."
          'rational
          `("Numerator: " (,(numerator rational)) ,newline
            "Denominator: " (,(denominator rational)) ,newline)))

(define-method &inspect-object ((integer :integer))
  (values "An integer."
          'integer
          (number-in-radices integer)))



;;; Indexed objects

(define-method &inspect-object ((vector :vector))
  (values "A vector."
          'vector
          `("Length: " (,(vector-length vector)) ,newline
            "Contents:" ,newline
            ,@(indexed-contents vector 0 (vector-length vector)
                                safe-vector-ref))))

(define (safe-vector-ref vector index)
  (if (vector-unassigned? vector index)
      "{unassigned}"
      `(,(vector-ref vector index))))

(define-method &inspect-object ((template :template))
  (values "A closure template (compiled code)."
          'template
          `("Length: " (,(template-length template)) ,newline
            ,@(template-header template)
            "Contents:" ,newline
            ,@(template-contents template)
            ,@(template-disassembly template))))

(define (template-disassembly template)
  (list newline "Disassembly:" newline
        (with-output-to-string
          (lambda ()
            (disassemble template)))))

(define (template-header template)
  `("Template ID: " (,(template-id template)) ,newline
    "Debug data: " (,(template-debug-data template)) ,newline
    "Package: " (,(template-package template)) ,newline))

(define (template-package template)
  (let ((uid (template-package-id template)))
    (or (uid->package uid)
        uid)))

(define (template-contents template)
  (append (indexed-contents template 0 template-overhead
                            safe-template-ref)
          (template-constant-contents template)))

(define (template-constant-contents template)
  (if (= (template-length template) template-overhead)
      '()
      `(" Constants:"                   ;Indent it slightly
        ,newline
        ,@(indexed-contents template
                            template-overhead
                            (template-length template)
                            safe-template-ref))))

(define (safe-template-ref template index)
  `(,(template-ref template index)))

(define (indexed-contents object start length ref)
  (reduce ((count* i start length))
      ((items '()))
    (append-reverse `(,i ,(ref object i) ,newline)
                    items)
    (reverse items)))

;;; Byte vectors we display very fancily.

(define-method &inspect-object ((bytev :code-vector))
  (values "A byte vector."
          'byte-vector
          (let ((len (byte-vector-length bytev)))
            `("Length: " (,len . ,(string-append
                                          (number->string len 10)
                                   " (#b" (number->string len 2)
                                   ", #o" (number->string len 8)
                                   ", #x" (number->string len 16)
                                   ")")) ,newline
              "Contents:" ,newline
              ,@(if (< len 16)
                    (reduce ((count* i 0 len))
                        ((items '()))
                      (append-reverse `(,i ,(string-pad
                                             (number->string
                                              (byte-vector-ref bytev i)
                                              16)
                                             2
                                             #\0)
                                           ,newline)
                                      items)
                      (reverse items))
                    (byte-vector-inspector-listing bytev))))))

(define (byte-vector-inspector-listing bytev)
  (let* ((len (byte-vector-length bytev))
         (hex-len (- (string-length (number->string len 16))
                     1)))
    (append-reverse
     ;; Build something like:
     ;;       0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
     (reduce ((count* i 0 16))
         ((headers (list (make-string (+ 3 hex-len) #\space)
                         newline)))
       (cons (string-append "  "
                            (number->string i 16))
             headers)
       (cons newline headers))
     (byte-vector-contents-listing bytev len hex-len))))

(define (byte-vector-contents-listing bytev len hex-len)
  (let outer-loop ((i 0) (contents '()))
    (let ((j (+ i 16)))
      (let inner-loop
          ((i i)
           (contents (cons* ":"
                            (string-pad (number->string
                                         (quotient i 16)
                                         16)
                                        hex-len
                                        #\0)
                            "  "
                            contents)))
        (cond ((= i len)
               (reverse (cons newline contents)))
              ((= i j)
               (outer-loop j (cons newline contents)))
              (else
               (inner-loop
                (+ i 1)
                (cons* (let ((byte (byte-vector-ref bytev i)))
                         (cons byte
                               (string-pad (number->string byte
                                                           16)
                                           2
                                           #\0)))
                       " "
                       contents))))))))

(define (cons* first . rest)
  (let recur ((x first) (rest rest))
    (if (pair? rest)
        (cons x (recur (car rest) (cdr rest)))
        x)))

(define (string-pad string width pad-char)
  (let ((len (string-length string)))
    (if (< len width)
        (string-append (make-string (- width len) pad-char)
                       string)
        string)))



;;; Pairs & lists

(define-method &inspect-object ((obj :null))
  (values "The empty list."
          'null
          '()))

(define-method &inspect-object ((pair :pair))
  (cond ((null? (cdr pair))         ; quick convenience for common case
         (values "A proper list."
                 'proper-list
                 `("Length: " (1) ,newline
                   "Contents:" ,newline
                   0 (,(car pair)) ,newline)))
        ((pair? (cdr pair))
         (inspect-list pair))
        (else
         (values "A pair."
                 'pair
                 `(car (,(car pair)) ,newline
                   cdr (,(cdr pair)) ,newline)))))

(define (inspect-list list)
  (let loop ((fast list) (slow list)
             (len 1)
             (contents `(,newline (,(car list)) 0)))
    (let ((fast-d (cdr fast)))
      (cond ((null? fast-d)
             (inspect-proper-list len contents))
            ((not (pair? fast-d))
             (inspect-dotted-list len contents fast-d))
            (else
             (let ((contents (append-reverse
                              `(,len (,(car fast-d)) ,newline)
                              contents))
                   (len (+ len 1))
                   (fast-dd (cdr fast-d)))
               (cond ((null? fast-dd)
                      (inspect-proper-list len contents))
                     ((not (pair? fast-dd))
                      (inspect-dotted-list len contents fast-dd))
                     (else
                      (let ((contents (append-reverse
                                       `(,len (,(car fast-dd))
                                              ,newline)
                                       contents))
                            (len (+ len 1))
                            (slow-d (cdr slow)))
                        (if (eq? fast-dd slow-d)
                            (inspect-circular-list list slow-d)
                            (loop fast-dd slow-d
                                  len contents)))))))))))

(define (inspect-proper-list len contents)
  (values "A proper list."
          'proper-list
          `("Length: " (,len) ,newline
            "Contents:" ,newline
              ,@(reverse contents))))

(define (inspect-dotted-list len contents tail)
  (values "A dotted list."
          'dotted-list
          `("Length: " (,len) ,newline
            "Contents:" ,newline
              ,@(reverse contents)
            cdr (,tail) ,newline)))

;++ I'm not sure that this code is quite correct.

(define (inspect-circular-list list cycle)
  (values "A circular list."
          'circular-list
          (let loop ((list list) (len-before 0) (contents '()))
            (if (eq? list cycle)
                (let loop1 ((list (cdr list))
                            (len-after (+ len-before 1))
                            (contents
                             (append-reverse `(,len-before
                                               "(cycle) "
                                               (,(car list))
                                               ,newline)
                                             contents)))
                  (if (eq? list cycle)
                      `("Length before cycle: " (,len-before) ,newline
                        "Length in cycle: " (,len-after) ,newline
                        "Contents:" ,newline
                          ,@(reverse contents))
                      (loop1 (cdr list)
                             (+ len-after 1)
                             (append-reverse `(,len-after
                                               (,(car list))
                                               ,newline)
                                             contents))))
                (loop (cdr list)
                      (+ len-before 1)
                      (append-reverse `(,len-before (,(car list))
                                                    ,newline)
                                      contents))))))



;;; General compound data

;** This code assumes that all environments are flat, which is the case
;** in Scheme48 1.3.  It would have to be changed if the environment
;** representation were to change.

(define-method &inspect-object ((closure :closure))
  (values "A closure."
          'closure
          (let ((template (closure-template closure))
                (env (closure-env closure)))
            `(,@(template-header template)
              "Template (length " (,(template-length template))
              "): " (,template) ,newline
              ,@(template-contents template)
              ,@(if (or (not env) (zero? (vector-length env)))
                    '()
                    `(,newline          ; Extra leading newline
                      "Environment:"
                      ,newline
                      ,@(inspect-environment
                         env
                         (debug-data-env-shape
                          (template-debug-data template)
                          #f))          ; No PC
                      ,newline))        ; Extra trailing newline
              ;++ What about components other than the template and the
              ;++ environment?  These can come up with peculiar hacks,
              ;++ and it might be nice to generalize to them.
              ,@(template-disassembly template)))))

(define (inspect-environment env shape)
  (let ((len (vector-length env)))
    (define (with-names i names result)
      (cond ((= i len)
             (reverse result))
            ((not (pair? names))
             (with-no-names i result))
            (else
             (with-names (+ i 1) (cdr names)
               (append-reverse `(,i ,(name-label (car names))
                                    ,(safe-vector-ref env i)
                                    ,newline)
                               result)))))
    (define (with-no-names i result)
      (if (= i len)
          (reverse result)
          (with-no-names (+ i 1)
            (append-reverse `(,i ,(safe-vector-ref env i) ,newline)
                            result))))
    (if (pair? shape)
        (with-names 0 (car shape) '())
        (with-no-names 0 '()))))

;;; This is messy for the sake of robustness.  People are apt to do
;;; pretty gross things with the low-level records layer.

(define-method &inspect-object ((record :record))
  (values "A record."
          'record
          (if (zero? (record-length record))
              `("(No contents.)" ,newline)
              (with-record-inspection record
                (lambda ()
                  (inspect-record record))))))

(define (with-record-inspection record thunk)
  ((call-with-current-continuation
     (lambda (k)
       (lambda ()
         (with-handler (lambda (condition propagate)
                         condition propagate
                         (k (lambda ()
                              (indexed-contents record
                                                0
                                                (record-length record)
                                                safe-record-ref))))
           thunk))))))

(define (safe-record-ref record index)
  `(,(record-ref record index)))

(define (inspect-record record)
  (let ((type (record-type record)))
    `("Type: " (,type) ,newline ,newline
      ,@(append-map (lambda (field-name)
                      `(,(name-label field-name)
                        (,((record-accessor type field-name)
                           record))
                        ,newline))
                    (record-type-field-names type)))))

(define (name-label name)
  (if (symbol? name)
      name
      (string-append (hybrid-write-to-string name)
                     ": ")))

;++ Argh, this is a hack that should not be necessary.
;++ What should the supertype be?  :VALUE or :RECORD?

(define-simple-type :table (:value) table?)

(define-method &inspect-object ((table :table))
  (values "A hash table."
          'table
          `("Size: " (,(table-size table)) ,newline
            ,@(let ((entries '()))
                (table-walk (lambda (key datum)
                              (set! entries
                                    (append-reverse
                                     `(,newline (,key) ,newline
                                                "  =>" ,newline
                                                (,datum) ,newline)
                                     entries)))
                            table)
                (reverse entries)))))

(define-simple-type :debug-data (:value) debug-data?)

(define-method &inspect-object ((ddata :debug-data))
  (values "A debugging information record."
          'debug-data
          (let ((id (debug-data-uid ddata))
                (names (debug-data-names ddata))
                (parent (get-debug-data (debug-data-parent ddata)))
                (env-maps (debug-data-env-maps ddata))
                (source (debug-data-source ddata)))
            `("Template ID: " (,id) ,newline
              "Names: " (,names) ,newline
              "Parent: " (,parent) ,newline
              "Environment maps: " (,env-maps) ,newline
              "Source: " (,source) ,newline))))

(define-method &inspect-object ((port :port))
  (values "A port."
          'port
          (let ((handler (port-handler port))
                (buffer (port-buffer port))
                (lock (port-lock port))
                (status (port-status port))
                (data (port-data port))
                (index (port-index port))
                (limit (port-limit port))
                (pending-eof? (port-pending-eof? port)))
            `(,@(inspect-port-status status)
              ,@(if pending-eof?
                    `("  (pending end of file)" ,newline)
                    '())
              "Buffer: " (,buffer) ,newline
              "  Index: " (,index) ,newline
              "  Limit: " (,limit) ,newline
              "Handler: " (,handler) ,newline
              "Data: " (,data) ,newline))))

;++ This cruft assumes lots about PORT-STATUS-OPTIONS; it shouldn't.

(define (inspect-port-status status)
  (let-syntax ((bit (syntax-rules ()
                      ((BIT enumerand description)
                       (IF (BIT-SET? (ENUM PORT-STATUS-OPTIONS
                                           enumerand)
                                     STATUS)
                           `("  (" description ")" ,NEWLINE)
                           '())))))
    `("Status: " (,status . ,(string-append
                              "#b"
                              (string-pad (number->string status 2)
                                          4
                                          #\0)))
      ,newline
      ,@(bit INPUT "input")
      ,@(bit OUTPUT "output")
      ,@(bit OPEN-FOR-INPUT "open for input")
      ,@(bit OPEN-FOR-OUTPUT "open for output"))))

(define (bit-set? bit integer)
  (= 1 (bitwise-and (arithmetic-shift 1 bit) integer)))



;;; Random utilities

(define (append-reverse list tail)
  (if (null? list)
      tail
      (append-reverse (cdr list)
                      (cons (car list) tail))))

(define (append-map fn list)
  (let loop ((in list) (out '()))
    (if (null? in)
        (reverse out)
        (loop (cdr in)
              (append-reverse (fn (car in))
                              out)))))

(define (string-upcase string)
  (let* ((len (string-length string))
         (result (make-string len)))
    (do ((i 0 (+ i 1)))
        ((= i len) result)
      (string-set! result i (char-upcase (string-ref string i))))))
