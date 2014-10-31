
;Andres G. Cavazos A01195067
;Gustavo Ferrufino A00812572

;1
;Implementar la función recursiva tokeniza reciba una lista posiblemente imbricada y
;la convierta en una lista plana que sustituya cada número por el símbolo N, cada
;símbolo por el símbolo S y cada sublista por una secuencia de símbolos I <elementos>
;D, donde <elementos> es una secuencia de elementos que depende del contenido de
;la sublista.

;(tokeniza '(1 a b 2))
;(tokeniza '(1 (a (2 b) 5)))
;(tokeniza '(1 b (a b)))

(define (tokeniza l)
   (cond [(null? l) ()]
         [(number? (first l)) (cons 'N (tokeniza (rest l)))]
         [(symbol? (first l)) (cons 'S (tokeniza (rest l)))]
         [ else (append (append (cons 'I (tokeniza (first l))) (list 'D )) (tokeniza (rest l)))]
         ))


;2
;Implementar la función recursiva subelementos que reciba una lista posiblemente
;imbricada y regrese otra lista con las mismas sublistas, pero sustituyendo los
;elementos por su cantidad en cada sublista.
;   (subelementos '(1 (2 (3 4) 5) (6 7))) => (3 (3 (2))(2))
;   (subelementos '(1 (2 (3 4))(5 (6 7)))) => (3 (2 (2))(2 (2)))
;   (subelementos '(1 2 3 4 5 6 7))



(define (subelementos l)
  (cons (length l) (sub l)
        ))

(define (sub l)
  (cond  ((null? l) ())
         ((list? (first l)) (cons (cons (length (first l)) (sub (first l))) (sub (rest l)) ))
         (else (sub (restt l)))
         ))



;5
;Implementar la función recursiva estadistica que a partir de la base de datos de
;equipos de futbol regrese una lista con los nombres de los equipos, su # de juegos
;jugados, la diferencia de goles y el # de puntos de cada uno, ordenada por # de
;puntos y en caso de empate en puntos, por diferencia de goles. Recordar que el
;número de puntos se calcula como 3 puntos por cada juego ganado y 1 por cada juego
;empatado.

;> (estadistica ligaMX) => (( 1 America 9 9 20 )( 2 Atlas 9 5 18 )
;( 3 Toluca 9 3 17 )( 4 Monterrey 8 5 16 )
;( 5 SantosLaguna 9 3 15 )( 6 Queretaro 9 3 14 )
;( 7 UANL 9 3 13 )( 8 Jaguares 9 1 13 )
;( 9 Pachuca 9 0 13 )( 10 Tijuana 9 2 11 )
;( 11 UNAM 9 0 11 )( 12 Puebla 9 -4 10 )
;( 13 Leon 9 -1 9 )( 14 CruzAzul 9 -3 9 )
;( 15 Guadalajara 8 -3 9 )( 16 Veracruz 9 -3 8 )
;( 17 LeonesNegros 9 -7 6 )( 18 Morelia 9 -13 3 ))

;nombre, 
;num de juegos jugados, 
;diferencia de goles 
;y el num de puntos de cada uno(3 g - 1 emp). 
;Ordenado por sus num d epuntos


(define estadistica
  (lambda (lista)
    (statsCallFuncs lista '() 1)))

(define statsCallFuncs  ; Enumera Tabla
  (lambda (lista listaAux pos)
    (if (null? lista)
        listaAux
        (statsCallFuncs (remve lista (order lista)) (append listaAux (list (append (list pos) (formt (order lista))))) (+ pos 1)))))

(define order
  (lambda (lista)
     (statsFunc lista '())))

(define statsFunc
  (lambda (lista b)
    (cond
      ((null? lista) b)
      ((null? b) (statsFunc (rest lista) (first lista)))
      ((< (points b) (points (first lista))) (statsFunc (rest lista) (first lista)))
      ((and (= (points b) (points (first lista))) (< (diffGoals b) (diffGoals (first lista)))) (statsFunc (rest lista) (first lista)))
      ((and (= (points b) (points (first lista))) (< (diffGoals b) (diffGoals (first lista)))) (statsFunc (rest lista) (first lista)))
      (else (statsFunc (rest lista) b)))))

(define formt
  (lambda (team)
    (list (first team) (+ (cadr team) (caddr team) (cadddr team)) (- (first (cddddr team)) (cadr (cddddr team))) (points team)))); suma partidos jugados y resta diferencia de goles

(define remve
  (lambda (lista object)
    (if (null? lista)
        '()
        (if (equal? (first lista) object)
            (rest lista)
            (cons (first lista) (remve (rest lista) object))))))

(define points
  (lambda (team)
    (if (null? team)
        0
        (+ (* (cadr team) 3) (caddr team)))))

(define diffGoals
  (lambda (team)
    (- (first (cddddr team)) (cadr (cddddr team)))
   ))


(define ligaMX '(( America 6 2 1 15 6 )( Atlas 5 3 1 13 8 )
( CruzAzul 2 3 4 7 10 )( Guadalajara 2 3 3 6 9 )
( Jaguares 3 4 2 13 12 )( Leon 3 0 6 15 16 )
( LeonesNegros 1 3 5 3 10 )( Monterrey 5 1 2 10 5 )
( Morelia 0 3 6 8 21 )( Pachuca 4 1 4 11 11 )
( Puebla 2 4 3 7 11 )( Queretaro 4 2 3 13 10 )
( SantosLaguna 4 3 2 12 9 )( Tijuana 2 5 2 11 9 )
( Toluca 5 2 2 11 8 )( UANL 3 4 2 14 11 )
( UNAM 3 2 4 12 12 )( Veracruz 1 5 3 5 8 )))