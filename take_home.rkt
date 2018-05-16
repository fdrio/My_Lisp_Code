#lang racket

;This code corresponds to the take home exam for the course CIIC4030 Section 066
; 1) identifiers: [a-zA-Z]+
; 2) delimiters: "("|")"|”[“|”]”
; 3) operators: "+"|"*"|”%”
; 4) integers: -?[0-9]+
; 5) whitespace: [ \n]+


; SAMPLE:
;   (INPUT) (my_lexer "2*[6%2]")
;   (OUTPUT) ((INT 2) (OP *) (LB) (INT 6) (OP module) (INT 2) (RB))



; Author Francisco Diaz Riollano
; Course: Porgramming Languages
; Prof. Wilson Rivera
; CIIC 4030-060



(define (categorize code)
  (cond
    ((null? code) '())
    ((and (= 1 (length code)) (char=? (car code) #\-)) (cons '(? #\-) (categorize (cdr code))))
    ((is_delimiter (car code)) (cons (is_delimiter (car code)) (categorize (cdr code))))
    ((char-alphabetic? (car code)) (cons (list 'ID (car code)) (categorize (cdr code))))
    ((char-numeric? (car code)) (cons (list 'INT (car code)) (categorize (cdr code))))
    ((char=? (car code) #\-) (if (char-numeric? (car (cdr code))) (cons '(INT #\-) (categorize (cdr code))) (cons '(? #\-) (categorize (cdr code)))))
    ((is_operator (car code)) (cons (is_operator (car code)) (categorize (cdr code))))
    ((is_whitespace (car code)) (cons (is_whitespace (car code)) (categorize (cdr code))))
    (else (cons (list '? (car code)) (categorize (cdr code))))
   )
 )


(define (my_lexer str)
  (display (append_char_list (merge_alphabetic (merge_whitespace (merge_integers (categorize (string->list str)))))))
 )


(define (tuple_refactor tuple)
  (cond
    ((null? tuple) '())
    ((= 1 (length tuple)) tuple)
    ((= 1 (length (cdr tuple))) (list (car tuple) (list->string (cdr tuple))))
    (else (list (car tuple) (list->string (cdr tuple))))
  )
 )


(define (is_operator c)
  (cond
    ((char=? c #\+) '(OP #\+))
    ((char=? c #\*) '(OP #\*)) 
    ((char=? c '#\%) '(OP #\m #\o #\d #\u #\l #\e))
    (else #F)
   )
)

(define (is_delimiter c)
  (cond
    ((char=? c #\() '(LP))
    ((char=? c #\)) '(RP))
    ((char=? c '#\[) '(LB))
    ((char=? c '#\]) '(RB))
    (else #F)
   )
)

(define (is_whitespace c)
  (if (char-whitespace? c) '(WS) #F)
)

(define (merge_alphabetic tuples)
  (cond
    ((null? tuples) '())
    ((= 1 (length tuples)) tuples)
    ((eq? '() (cdr tuples)) (car tuples))
    ((and (eq? 'ID (car (car tuples))) (eq? 'ID (car (car (cdr tuples))))) (merge_alphabetic (cons (cons 'ID (append (cdr (car tuples)) (cdr (car (cdr tuples))))) (cdr (cdr tuples)))))
    (else (cons (car tuples) (merge_alphabetic (cdr tuples))))
  )
 )
(define (merge_integers tuples)
  (cond
    ((null? tuples) '())
    ((= 1 (length tuples)) tuples)
    ((eq? '() (cdr tuples)) (car tuples))
    ((and (eq? 'INT (car (car tuples))) (eq? 'INT (car (car (cdr tuples)))) (not (eq? #\- (car (cdr (car (cdr tuples))))))) (merge_integers (cons (cons 'INT (append (cdr (car tuples)) (cdr (car (cdr tuples))))) (cdr (cdr tuples)))))
    (else (cons (car tuples) (merge_integers (cdr tuples))))
  )
 )
(define (merge_whitespace tuples)
  (cond
    ((null? tuples) '())
    ((= 1 (length tuples)) tuples)
    ((and (eq? 'WS (car (car tuples))) (eq? 'WS (car (car (cdr tuples))))) (merge_whitespace (cdr tuples)))
    (else (cons (car tuples) (merge_whitespace (cdr tuples))))
  )
 )

(define (append_char_list tuples)
  (cond
   ((null? tuples) '())
   (else (cons (tuple_refactor (car tuples)) (append_char_list (cdr tuples))))
  )
  )


