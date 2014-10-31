;nprocs 
(define (cuadrado x) (* x x))
(define inc1 (lambda (x) (+ x 1)))

(define count_lambda
  (lambda (y)
    (cond ((null? y) 0) ; is the list empty?
          ((not (pair? y)) ; if the current element is an atom
           (if (eq? y 'lambda) 1 0)) ; then check to see if is a lambda
          (else (+ (count_lambda (car y)) ; otherwise advance recursion
                   (count_lambda (cdr y)))))))


(define (nprocs sxp)
  (cond
    ; cons cell -> process car and cdr
    ((pair? sxp) (+ (nprocs (car sxp)) (nprocs (cdr sxp))))
    ; atom -> is it a procedure that is not a primitive?
    ((and (procedure? sxp) (not (primitive? sxp))) 1)
    ; atom, not or procedure or a primitive
    (else 0)))


;Mapea
(define (aplica proc a)
  (proc a))

(define (mapea op l)
   (cond ( (null? l) '() )
        (else ( cons (aplica op (car l)) ( mapea op (cdr l) )))
        ))

