#lang racket/base
(require racket/class
         racket/gui/base
         racket/list
         racket/string
         racket/contract)
(provide (all-defined-out))

(module+ test
  ;; todo: fix this so it can be tested on travis
  (require racket/gui/base rackunit)
  (define t (new text%))
  (define t-str "foo\n ar\n  m")
  (define result (send t insert-port (open-input-string t-str))))

(define indent-width 2)

(define/contract (char text pos)
  ((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c char? #f))
  (and pos (let ([c (send text get-character pos)])
             (if (char=? #\nul c) #f c))))

(module+ test
  (check-equal? (char t 0) #\f)
  (check-equal? (char t 2) #\o)
  (check-equal? (char t 3) #\newline)
  (check-equal? (char t 10) #\m)
  (check-equal? (char t 11) #f))

(define/contract (line text pos)
  ((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . exact-nonnegative-integer?)
  (send text position-line pos))

(module+ test
  (check-equal? (line t 0) 0)
  (check-equal? (line t 2) 0)
  (check-equal? (line t 5) 1)
  (check-equal? (line t 10) 2)
  (check-equal? (line t 11) 2))

(define/contract (line-chars text line)
  ((is-a?/c text%) exact-nonnegative-integer? . -> . (or/c (listof char?) #f))
  (and
   (valid-line? text line)
   (for*/list ([pos (in-range (line-start text line) (add1 (line-end text line)))]
               [c (in-value (char text pos))]
               #:when c)
              c)))

(module+ test
  (check-equal? (line-chars t 0) '(#\f #\o #\o #\newline))
  (check-equal? (line-chars t 1) '(#\space #\a #\r #\newline))
  (check-equal? (line-chars t 2) '(#\space #\space #\m))
  (check-equal? (line-chars t 3) #f))


(define/contract (previous-line text pos)
  ((is-a?/c text%) exact-nonnegative-integer? . -> . (or/c exact-nonnegative-integer? #f))
  (define this-line (line text pos))
  (and (this-line . > . 0) (sub1 this-line)))

(module+ test
  (check-equal? (previous-line t 0) #f)
  (check-equal? (previous-line t 2) #f)
  (check-equal? (previous-line t 5) 0)
  (check-equal? (previous-line t 10) 1)
  (check-equal? (previous-line t 11) 1))

(define/contract (next-line text pos)
  ((is-a?/c text%) exact-nonnegative-integer? . -> . (or/c exact-nonnegative-integer? #f))
  (define last (send text last-line))
  (define this-line (line text pos))
  (and (this-line . < . last) (add1 this-line)))

(module+ test
  (check-equal? (next-line t 0) 1)
  (check-equal? (next-line t 2) 1)
  (check-equal? (next-line t 5) 2)
  (check-equal? (next-line t 10) #f)
  (check-equal? (next-line t 11) #f))

(define/contract (valid-line? text line)
  ((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . boolean?)
  (and line (<= 0 line (send text last-line))))

(define/contract (line-start text line)
  ((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c exact-nonnegative-integer? #f))
  (and (valid-line? text line)
       (send text line-start-position line)))

(module+ test
  (check-equal? (line-start t 0) 0)
  (check-equal? (line-start t 1) 4)
  (check-equal? (line-start t 2) 8)
  (check-equal? (line-start t 3) #f))
  
(define/contract (line-end text line)
  ((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c exact-nonnegative-integer? #f))
  (and (valid-line? text line)
       (send text line-end-position line)))

(module+ test
  (check-equal? (line-end t 0) 3)
  (check-equal? (line-end t 1) 7)
  (check-equal? (line-end t 2) 11)
  (check-equal? (line-end t 3) #f))

(define (first-visible-char-pos text start end)
  ;; private
  (for*/first ([pos (in-range start end (if (start . > . end) -1 1))]
               [c (in-value (char text pos))]
               #:when (not (char-blank? c)))
              pos))

(define/contract (line-start-visible text line)
  ((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c exact-nonnegative-integer? #f))
  (define start (line-start text line))
  (define end (line-end text line))
  (and start end (first-visible-char-pos text start end)))

(module+ test
  (check-equal? (line-start-visible t 0) 0)
  (check-equal? (line-start-visible t 1) 5)
  (check-equal? (line-start-visible t 2) 10)
  (check-equal? (line-start-visible t 3) #f))

(define/contract (line-first-visible-char text line)
  ((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c char? #f))
  (char text (line-start-visible text line)))

(define/contract (line-last-visible-char text line)
  ((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c char? #f))
  (char text (line-end-visible text line)))  

(define/contract (line-end-visible text line)
  ((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c exact-nonnegative-integer? #f))
  (define start+1 (line-end text line)) ; start before newline
  (define end+1 (line-start text line))
  (and start+1 end+1 (first-visible-char-pos text (sub1 start+1) (sub1 end+1))))

(module+ test
  (check-equal? (line-end-visible t 0) 2)
  (check-equal? (line-end-visible t 1) 6)
  (check-equal? (line-end-visible t 2) 10)
  (check-equal? (line-end-visible t 3) #f))

(define/contract (line-indent text line)
  ((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . exact-nonnegative-integer?)
  (and (valid-line? text line)
       ;; though lsv can be #false (because a line may not have any starting char)
       ;; an indent is always, at worst, zero
       (let ([lsv (line-start-visible text line)])
         (if lsv (- lsv (line-start text line)) 0))))

(module+ test
  (check-equal? (line-indent t 0) 0)
  (check-equal? (line-indent t 1) 1)
  (check-equal? (line-indent t 2) 2)
  (check-equal? (line-indent t 3) #f))

(define (count-char text c [start 0] [end (send text last-position)])
  (for*/sum ([pos (in-range start (add1 end))]
             [d (in-value (char text pos))]
             #:when (and d (d . char=? . c)))
            1))

(module+ test
  (check-equal? (count-char t #\f) 1)
  (check-equal? (count-char t #\o) 2)
  (check-equal? (count-char t #\o 0 1) 1)
  (check-equal? (count-char t #\newline) 2))


(define (str->text str)
  (define t (new text%))
  (send t insert-port (open-input-string str))
  t)

(define (space-char? x) (char=? x #\space))


(define/contract (apply-indenter indenter t-or-str)
  (procedure? (or/c (is-a?/c text%) string?) . -> . string?)
  (define t (if (string? t-or-str) (str->text t-or-str) t-or-str))
  (define indented-t
    (for/fold ([t-acc t])
              ([line-idx (in-range (add1 (send t last-line)))])
      ;; simulate DrR indentation
      ;; by dropping leading spaces and applying new indent.
      (define line-start-pos (line-start t-acc line-idx))
      (define new-indent (indenter t-acc line-start-pos))
      (define new-line-str
        (list->string (append (make-list (or new-indent 0) #\space)
                              (dropf (line-chars t-acc line-idx) space-char?))))
      (send t-acc delete line-start-pos (add1 (line-end t-acc line-idx))) ; add1 to grab ending newline too
      (send t-acc insert new-line-str line-start-pos)
      t-acc))
  (send indented-t get-text))

(define/contract (string-indents str)
  (string? . -> . (listof exact-nonnegative-integer?))
  (for/list ([line (in-list (string-split str "\n"))])
            (length (takef (string->list line) space-char?))))

(module+ test
  (check-equal? (string-indents t-str) '(0 1 2)))