;1.-
(define (add-ren l1 l2)
  (cond ([null? l1] (cons l2 '()))
        (else 
          (append l1 (list l2)))
        ))
;2.- Esto esta mal
(define (add-col l1 l2)
  (cond ([null? l1] (cons (list (car l2))(list (cdr l2))))
        (else 
          (append(append (car l1) (list (car l2)))
          (append (list (cdr l1)) (list (cdr l2)))))
        ))
;3.-
(define rp '((a0111111 (Jorge Perez) (100 100 100))
              (a0222222 (Gloria Flores) (90 80 100))
              (a0333333 (Ramiro Mendez) (90 60 90))))

;Crea lista de dos atomos
(define (returnlist a b) 
    (cons a (cons b '())))

;Devuelve la matricula con el promedio del alumno
(define (prom lst)
  (returnlist (car lst)(/(sum (caddr lst)) 3)))

;sum 
(define (sum elemList)
  (if
    (null? elemList)
    0
    (+ (car elemList) (sum (cdr elemList)))
  )
)

;principal
(define (promedio lis)
   (cond ((null? lis) '())
         (else
          (cons (prom (car lis)) 
                (promedio (cdr lis)) ) )
         ))


;4.-
;; returns value of node
(define (value node)
  (if (null? node) '()
      (car node)))

;; returns left subtree of node
(define (left node)
  (if (null? node) '()
      (cadr node)))

;; returns right subtree of node
(define (right node)
  (if (null? node) '()
      (caddr node)))
;;leaves
(define (leaves tree)
        (if (and (null? (left tree)) (null? (right tree)))
            (list (value tree))
            (append (leaves (left tree)) (leaves (right tree)))))

;;delete parentheses
(define (delete atm lis)
  (cond
   ((null? lis) '())
   ((eq? atm (car lis)) (delete atm (cdr lis)))
   (else (cons (car lis) (delete atm (cdr lis))))))

;;Principal
(define (hojas arbol)
  (delete '() (leaves arbol))
  )
