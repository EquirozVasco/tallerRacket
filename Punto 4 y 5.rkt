#lang racket
;definir el nodo
(define (nodo dato izq der)
  (list dato izq der)
  )
;retornar el dato
(define (raiz ab)
  (car ab))
;retornar el subarbol izquierdo
(define (izquierdo ab)
  (cadr ab))
;retornar el subarbol derecho
(define (derecho ab)
  (caddr ab))
;validar si el arbol esta vacio
(define (vacio-ab? ab)
  (null? ab))
;validar si un nodo es una hoja
(define (hoja-ab? ab)
  (and (vacio-ab? (derecho ab))
       (vacio-ab? (izquierdo ab))
  )
  )
;Llevar a una nueva lista los datos de un árbol binario in-orden.
(define (mostrar-lista-ab ab)
  (if (vacio-ab? ab)
      '()
      (append
       (mostrar-lista-ab (izquierdo ab))
       (list (raiz ab))
       (mostrar-lista-ab (derecho ab))
       )
   )
  )
;Asumiendo un BST(binary search tree), implementar función member.
;buscar dato, verdadero si lo encuentra y falso si no.

(define (buscar-ab? x bst)
  (cond
    [(vacio-ab? bst) #f]
    [(= x (raiz bst)) #t]
    [(< x (raiz bst)) (buscar-ab? x (izquierdo bst))]
    [(> x (raiz bst)) (buscar-ab? x (derecho bst))]
    )
  )

;agregar un dato al arbol binario
;crear un nuevo arbol a partir de uno existente

(define (agregar-dato-ab x bst)
  (cond
    [(vacio-ab? bst) (nodo x null null)]
    [(< x (raiz bst))
     (nodo
      (raiz bst)
      (agregar-dato-ab x (izquierdo bst))
      (derecho bst)
      )
     ]
    [(> x (raiz bst))
     (nodo
      (raiz bst)
      (izquierdo bst)
      (agregar-dato-ab x (derecho bst))
      )
     ]
    [else bst]
    )
)

;agregar una lista de datos

(define (agregar-lista-ab lista bst)
  (if (vacio-ab? lista)
      bst
      (agregar-lista-ab (cdr lista) (agregar-dato-ab (car lista) bst))
      )
  )

;realizar la sumatoria de elementos del arbol
;

;Variable is only one piece of data, a method is basically instructions in which the variables act

;arbol de prueba expresion matemática
(define m3 (nodo 4 null null))
(define m2 (nodo 3 null null))
(define m1 (nodo '* m2 m3))

;arbol de prueba BST
(define n8 (nodo 6 null null))
(define n7 (nodo 9 null null))
(define n6 (nodo 18 null null))
(define n5 (nodo 22 null null))
(define n4 (nodo 8 n8 n7))
(define n3 (nodo 20 n6 n5))
(define n2 (nodo 17 null n3))
(define n1 (nodo 12 n4 n2))



; Task #1 point-4 Solution:


(define (Sorry arbolito)
  (cond
    [(vacio-ab? arbolito) 0]
    [else (+ 1 (Sorry (izquierdo arbolito)) (Sorry (derecho arbolito)))]
   )

)
(define (Teacher arbolito)
  (cond
    [(vacio-ab? arbolito) 0]
    [(hoja-ab? arbolito) 1]
    [else (+ (Teacher (izquierdo arbolito)) (Teacher (derecho arbolito)))]


    )
  
)


;Point 5, task #1




(define (Lvl hoja arbolito)
  (cond
    [(= hoja (raiz arbolito)) 0]
    [(< hoja (raiz arbolito)) (+ 1 (Lvl hoja (izquierdo arbolito)))]
    [(> hoja (raiz arbolito)) (+ 1 (Lvl hoja (derecho arbolito)))]

   

    )



  )

(define (Height tree realtree)
  (cond
    [(vacio-ab? tree) '()]
    [(hoja-ab? tree) (append (list (Lvl (raiz tree) realtree)))]
    [else (append (Height (izquierdo tree) realtree) (Height (derecho tree) realtree))]
  )
)

(define (compare-tree lista)
  (cond
    [(empty? (cdr lista)) #t]
    [(not (equal? (car lista) (car (cdr lista)))) #f]
    [else (compare-tree (cdr lista))]
  )
)

(define (ab-balanceado tree)
  (compare-tree (Height tree tree))
)