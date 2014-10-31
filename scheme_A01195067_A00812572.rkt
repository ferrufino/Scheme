#lang racket
; Andres Cavazos     a01195067
; Gustavo Ferrufino  a00812572

; 1) Predicado que recibe 3 argumentos numericos y determine si fueron en orden no decreciente

(define (decreciente? v1 v2 v3)
  (if (> v1 v2 v3) #t #f))

; 2) Funcion que reciba 3 argumentos numericos y regrese el mayor

(define (mayor v1 v2 v3)
  (cond
    ((and (> v1 v2) (> v1 v3)) v1)
    ((and (> v2 v1) (> v2 v3)) v2)
    (else v3)))

; 3) Funcion que reciba 4 argumentos numericos y regrese un simbolo que indique si hay mas pares, mas nones o un empate

(define (paronon v1 v2 v3 v4)
  (if(> (+(if (= (remainder v1 2) 0) 2 1)
           (if (= (remainder v2 2) 0) 2 1)
           (if (= (remainder v3 2) 0) 2 1)
           (if (= (remainder v4 2) 0) 2 1)) 6) 'pares (if(< (+(if (= (remainder v1 2) 0) 2 1)
                                                              (if (= (remainder v2 2) 0) 2 1)
                                                              (if (= (remainder v3 2) 0) 2 1)
                                                              (if (= (remainder v4 2) 0) 2 1)) 6) 'nones 'empate)))
