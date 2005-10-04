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
  (parts         ; initialized by SWANK:INSPECTOR-REINSPECT
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

(define (swank:init-inspector exp-string)
  (cond ((repl-eval-string exp-string)
         => inspect-results)
        (else (abort-swank-rpc))))

(define (swank:inspector-nth-part n)
  (xvector-ref (current-inspector-parts) n))

(define (swank:inspect-nth-part n)
  (inspect-subobject (swank:inspector-nth-part n)))

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

(define (inspect-results results)
  (inspect-object (cond ((null? results)
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
    (receive (contents parts)
             (process-inspector-listing listing)
      (set-current-inspector-parts! parts)
      `(:TITLE   ,title
        :TYPE    ,(string-upcase (symbol->string type))
        :CONTENT ,contents))))

(define (process-inspector-listing listing)
  (let ((parts (make-xvector)))
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
                              (limited-write-to-string obj
                                (inspector-depth)
                                (inspector-length)))
                         ,(xvector-push! parts obj))))
             (else
              (error "invalid inspection listing item"
                     item))))

      (values (reverse contents) parts))))

(define (symbol-label symbol)
  (string-append (string-upcase (symbol->string symbol))
                 ": "))

(define (integer-label integer)
  (string-append "  "
                 (number->string integer 10)
                 ": "))

(define-generic really-inspect-object &inspect-object (object))

(define-method &inspect-object (obj)
  (values "An indeterminate object."
          'object
          `(,(circular-write-to-string obj))))

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
              (append-reverse `(,i (,val)) contents)

              (reverse contents)))))



;;; Miscellaneous data

(define-method &inspect-object ((symbol :symbol))
  (values "A symbol." 'symbol
          `("String: " (,(symbol->string symbol)))))

(define-method &inspect-object ((string :string))
  (values "A string." 'string `("\"" ,string "\"")))

(define-method &inspect-object ((char :char))
  (values "A character." 'char `("#\\" ,char)))

(define-method &inspect-object ((boolean :boolean))
  (values "A boolean." 'boolean `(,(if boolean "#t" "#f"))))

(define-method &inspect-object ((obj :eof-object))
  (values "The end of file object." 'eof-object '()))

(define-method &inspect-object ((loc :location))
  (values "A location (top-level variable cell)."
          'location
          `("Contents: " (,(contents loc)))))

(define-method &inspect-object ((cell :cell))
  (values "A cell."
          'cell
          `("Value: " ,(if (cell-unassigned? cell)
                           "{unassigned}"
                           (list (cell-ref cell))))))

(define-method &inspect-object ((weak :weak-pointer))
  (values "A weak pointer."
          'weak-pointer
          `("Ref: " (,(weak-pointer-ref weak)))))



;;; Numbers

(define (number-in-radices number)
  `(,(number->string number) ,newline
    "Binary: #b" ,(number->string number 2) ,newline
    "Octal: #o" ,(number->string number 8) ,newline
    "Decimal: #d" ,(number->string number 10) ,newline
    "Hexadecimal: #x" ,(number->string number 16)))

(define-method &inspect-object ((number :number))
  (values "A number."
          'number
          (number-in-radices number)))

(define-method &inspect-object ((complex :complex))
  (values "A complex number."
          'complex
          `("Real part: " (,(real-part complex)) ,newline
            "Imaginary part: " (,(imag-part complex)))))

(define-method &inspect-object ((real :real))
  (values "A real number."
          'real
          ;++ This should show the underlying raw flonum bit string.
          (number-in-radices real)))

(define-method &inspect-object ((rational :rational))
  (values "A rational number."
          'rational
          `("Numerator: " (,(numerator rational)) ,newline
            "Denominator: " (,(denominator rational)))))

(define-method &inspect-object ((integer :integer))
  (values "An integer."
          'integer
          (number-in-radices integer)))



;;; Indexed objects

(define (define-indexed-inspector :type title type-id
          ref length
          unassigned?)
  (define-method &inspect-object ((obj :type))
    (values title type-id
            (let ((len (length obj)))
              `("Length: " (,len) ,newline
                "Contents:"
                  ,@(reduce ((count* i 0 len))
                        ((items '()))
                      (append-reverse `(,newline ,i
                                        ,(if (unassigned? obj i)
                                             "{unassigned}"
                                             (list (ref obj i))))
                                      items)

                      (reverse items)))))))

(define-indexed-inspector :vector "A vector." 'vector
  vector-ref vector-length
  vector-unassigned?)           ; may be the case in environments

(define-indexed-inspector :template "A template (compiled code)."
  'template
  template-ref template-length
  (lambda (t i) #f))

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
              "Contents:"
              ,@(if (< len 16)
                    (reduce ((count* i 0 len))
                        ((items '()))
                      (append-reverse `(,newline ,i
                                        ,(string-pad
                                          (number->string
                                           (byte-vector-ref bytev i)
                                           16)
                                          2
                                          #\0))
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
               (reverse (if (= i j) (cons newline contents) contents)))
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
                   cdr (,(cdr pair)))))))

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
            cdr (,tail))))

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



;;; Random utilities & parameters

(define (inspector-depth) 4)
(define (inspector-length) 5)

(define (append-reverse list tail)
  (if (null? list)
      tail
      (append-reverse (cdr list)
                      (cons (car list) tail))))

(define (string-upcase string)
  (let* ((len (string-length string))
         (result (make-string len)))
    (do ((i 0 (+ i 1)))
        ((= i len) result)
      (string-set! result i (char-upcase (string-ref string i))))))
