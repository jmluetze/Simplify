;Written mainly by Jonathan Luetze on 2/7/2018
;Rules 15 - 20 written by Ashley Tran and Tiana Terrell
;
;The purpose of this program is to simplify input according to 20 different rules
;
;If something is not an integer, it is a symbol or another list that will be sent
;recursively through the simplify function until no further reductions can be made
;
;
;Defining what a constant is to differentiate the rules
(define (constant? x) (number? x))
;
;To check for list and the operator of the list
(define (is-sum x)
  (and (pair? x) (equal? (car x) '+)))
(define (is-product x)
  (and (pair? x) (equal? (car x) '*)))
(define (is-difference x)
  (and (pair? x) (equal? (car x) '-)))
;
;To check for a legal operation
(define (is-operation x)
  (or (is-sum x) (is-product x) (is-difference x)))
;
;To check for the correct operator
(define (is-plus x)  (equal? x '+))
(define (is-minus x) (equal? x '-))
(define (is-mul x)   (equal? x '*))
;
;To retrieve the right and left side of a list within a list
(define (rightSide x) (caddr x))
(define (leftSide x)  (cadr x))
;
;To see if both args are constants
(define (2const? op arg1 arg2)
      (and (constant? arg1) (constant? arg2)))
;
;Function to apply all rules
(define (apply-rules op arg1 arg2)
￼
  (cond
  ;Rules 1,3,5
   ((2const? op arg1 arg2)
       (cond   ((is-plus op)  (+ arg1 arg2))
               ((is-minus op) (- arg1 arg2))
               ((is-mul op)   (* arg1 arg2))
) )
  ;Rules 2,4,6
   ((constant? arg2)
       (cond   ((is-plus op)  (simplify (list '+ arg2 arg1)))
               ((is-mul op)   (simplify (list '* arg2 arg1)))
               ((is-minus op) (list '+ (list '- arg2) arg1))
) )
  ;Rules 7,9
   ((and (is-plus op) (is-sum arg2))
       (simplify(list '+ (list '+ arg1 (leftSide arg2)) (rightSide arg2)))
   )
  ;Rules 8,10
   ((and (is-mul op) (is-product arg2))
       (simplify(list '* (list '* arg1 (leftSide arg2)) (rightSide arg2)))
   )
   ;Rules 11-14, 18
   ((and (is-mul op) (is-sum arg2))
       (simplify(list '+ (list '* arg1 (leftSide arg2)) (list '* arg1 (rightSide
arg2))))
   )
   ;Rules 15,16
   ((and (is-mul op) (is-difference arg2)(constant? arg1))
       (simplify(list '- (list '* arg1 (leftSide arg2)) (list '* arg1 (rightSide
arg2))))
   )
   ;Rule 17
   ((and (is-mul op) (is-sum arg1))
           (simplify (list '+ (list '* (leftSide arg1) arg2) (list '* (rightSide arg1)
arg2))) )
;Rule 19
((and (is-mul op) (is-difference arg1))
           (simplify (list '- (list '* (leftSide arg1) arg2) (list '* (rightSide arg1)
arg2))) )
   ;Rule 20
   ((and (is-mul op) (is-difference arg2))
           (simplify (list '- (list '* arg1 (leftSide arg2)) (list '* arg1 (rightSide
arg2)))) )
   ;Else return list
   (else (list op arg1 arg2))
  )
)
;Main simplify function that recursively simplifies the equations
(define (simplify exp)
  (cond ((is-operation exp)
           (apply-rules (car exp)
) )
(else exp) )
)
