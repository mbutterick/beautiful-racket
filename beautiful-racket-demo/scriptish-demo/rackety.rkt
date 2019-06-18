#lang br
;
;var x = 42;
(define x 42)
;var s = "string";
(define s "string")
;
;x + x;
(define (add/concat . xs)
  (cond
    [(andmap number? xs) (apply + xs)]
    [(ormap string? xs) (string-join (map ~a xs) "")]))
(add/concat x x)
;s + x;
(add/concat s x)
;
;var thing = {
;    'foo' : 42,
;
;    'bar' : function(x) {
;        return x + 15;
;    }
;};

(define thing (hash
               "foo" 42
               "bar" (λ (x) (let/ec return (return (add/concat x 15)) (void)))))

;thing.foo
;thing.bar
;thing.bar(3)

(hash-ref thing "foo")
(hash-ref thing "bar")
(#%app (hash-ref thing "bar") 3)

;
;if ( thing.foo == 42 ) {
;    console.log("The correct answer is " + thing.foo);
;}

(when (equal? (hash-ref thing "foo") 42)
  (displayln (add/concat "The correct answer is " (hash-ref thing "foo")))) 

;var idx = 0;
;while ( idx != 50 ) {
;    if ( thing.bar(idx) == 35 ) {
;       alert("Calamity at " + idx + "!");
;    }
;    idx++;
;}

(define (alert str)
  (displayln "*********")
  (displayln str)
  (displayln "*********"))

(define idx 0)
(let loop ()
  (when (not (equal? idx 50))
    (when (equal? (#%app (hash-ref thing "bar") idx) 35)
      (alert (add/concat "Calamity at " idx "!")))
    (set! idx (add1 idx))
    (loop)))