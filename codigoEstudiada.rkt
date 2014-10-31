; MERGE LISTS
(define (merge l1 l2)
      (if (null? l1) l2
          (if (null? l2) l1
              (cons (car l1) (cons (car l2) (merge (cdr l1) (cdr l2)))))))

;ORDER ASCD
(define (selection L) 
   (cond ( (null? L) '() )
         ( else (cons (smallest L (car L))     ; put the smallest element
                                               ; at the front of the 
                                               ; current list 
                      (selection (rem L (smallest L (car L)))))
                                               ; call selection on the list
                                               ; minus the smallest
                                               ; element
         )
   )
)
(define (rem L A)                ; remove the first occurance of atom A from L
  (cond ( (null? L) '() )           
        (  ( = (car L) A) (cdr L))    ; Match found! 
        (else (cons (car L)(rem (cdr L) A)))   ; keep searching
  )
)

(define (smallest L A)             ; looks for the smallest element in the list
                                   ; atom A is the current smallest
  (cond ( (null? L) A)
        ( (< (car L) A) (smallest (cdr L)(car L)))
        (else (smallest (cdr L) A ))
  )
)

;REMOVE DUCPLICATED
(define (remove-duplicated list)
  ;; remove duplicates from a *sorted* list.  Because the 
  ;; list is sorted, any duplicates of an element will
  ;; immediately follow the first occurrence of the element.
  ;;---------------------------------------------------------
  ;; If the list has the form () or (x)
  (if (or (null? list)
          (null? (cdr list)))
      ;; then it has no duplicates, so return it
      list
      ;; otherwise, if the list looks like (x x ...)
      (if (= (car list) (cadr list))
          ;; then you can discard the first element, but you
          ;; still need to remove duplicates from the rest of
          ;; the list, since there can be more duplicates later
          (remove-duplicated (cdr list))
          ;; otherwise, you need the first element of the list
          ;; and can simply remove-duplicated from the rest.
          (cons (car list) (remove-duplicated (cdr list))))))  

;METODO PRINCIPAL
(define (mezcla l1 l2)
    (remove-duplicated (selection (merge l1 l2))))