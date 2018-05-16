#lang racket

;(define(factorial n)
;(if (= n 0)
;1
;(* n (factorial(- n 1)))
;
;
;))
;
;(define (sumlist x)
;(cond
;  ((null? x) 0)
;  (else (+ (car x) (sumlist(cdr x))))
;))
;
;
;(define (power A B)
;(if(= B 0)
;   1
;   (* A (power A (- B 1)))
;))
;
;
;(define(append list1 list2)
;  (cond
;    ((null? list1) list2)
;    (else(cons (car list1) (append (cdr list2)))
;   )
;  )
; )
;
;
;(define (removelast list1)
;  (cond
;    ((null? (cdr list1))'())
;    (else (cons (car list1) (removelast (cdr list1) )))
;  )
;)
;
;(define (rev l)
;    (if (null? (cdr l))
;        l
;        (append (rev (cdr l)) (list (car l)))))

(define (op ch)
  (cond
    ((eq? ch #\+) '(OP +))
    ((eq? ch #\*) '(OP *))
    ((eq? ch '#\%) '(OP %))
    (else #f)
   )
)


(define (del ch)
  (cond
    ((eq? ch #\() '(LP))
    ((eq? ch #\)) '(RP))
    ((eq? ch '#\[) '(LB))
    ((eq? ch '#\]) '(RB))
    (else #f)
   )
)

(define (newl ch)
  (cond
    ((eq? ch #\newline) '(\n))
    (else #f)
   )
)


;  '((INT 1)(INT 2)(INT 3))
(define(merge_int_list lst)
  (cond
    ((null? (car lst)) '())
    ((eq? (car(car lst)) 'INT )  (append(cdr (car lst))) (merge_int_list (cdr lst)))
    (else (append (car lst)(merge_int_list(cdr lst))))

   )
)




