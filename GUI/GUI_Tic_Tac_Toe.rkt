#lang racket
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TicTacToe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/gui)

;Integrantes:
;Michael Valverde Navarro
;Isaac Herrera Monge
;Naheem Johnson Solís

;Se define el frame para la GUI
(define frame_GUI null)
(define width 600)
(define height 600)
(define sqrSize 0)
(define px 0)
(define py 0)
(define mp 0)
(define np 0)
(define a_list '())
(define gameBoard '())

;Definiciones para dibujar en el gameboard
(define red-marker (make-object pen% "RED" 4 'solid))
(define blue-marker (make-object pen% "BLUE" 4 'solid))
(define black-marker (make-object pen% "BLACK" 4 'solid))

(define x-simbol(make-object bitmap% "x.png"))

(define (calcularSqr max)
  (set! sqrSize (/ 600 max)))

(define (aproxPos x)
  (floor ( / x sqrSize)))

;Creación de canvas para manejar eventos
(define frame_canvas%
  (class canvas%
    ;Override del método para manejar eventos
    (define/override (on-event event)
      (cond ((send event button-down? 'left)
             (set! px(aproxPos(send event get-x)))
             (set! py(aproxPos(send event get-y))))))))
             

;Para dibujar en el gameBoard

(define (drawColumn fila columna cantidadLineas cantidadColumnas dc)
  (send dc set-pen black-marker)
  (cond ((equal? columna cantidadColumnas))
        (else (let ()
                (drawLine fila columna cantidadLineas cantidadColumnas dc)
                (send dc draw-rectangle(* cantidadLineas sqrSize)(* cantidadColumnas sqrSize)sqrSize sqrSize)
                (drawColumn fila columna cantidadLineas(+ 1 cantidadColumnas)dc)))))


(define (drawLine fila columna cantidadLineas cantidadColumnas dc)
  (cond ((equal? fila cantidadLineas))
        (else (let()
                (send dc draw-rectangle (* cantidadLineas sqrSize) (* cantidadColumnas sqrSize) sqrSize sqrSize)
                (drawLine fila columna (+ 1 cantidadLineas ) cantidadColumnas dc)))))


;funciones para dibujar en canvas
(define (drawX x y dc)
  (send dc set-pen red-marker)
  (send dc draw-line x y (-(+ x sqrSize)10) (-(+ y sqrSize)10))
  (send dc draw-line x (-(+ y sqrSize)10)(-(+ x sqrSize)10)y))


(define (drawO x y dc)
  (send dc set-pen blue-marker)
  (send dc draw-ellipse x y (- sqrSize 10)(- sqrSize 10)))

(define (drawEle px py dc type)
  (cond ((equal? type 0)
         (drawX (+(* sqrSize px)(+ sqrSize py)5)dc))
        ((equal? type 1)
         (drawO (+(* sqrSize px)5)(+(* sqrSize py)5)dc))))


;Definiciones que permiten generar la matriz inicial para el gameboard
(define (makeTable x y M N matrix)
  (cond ((equal? x N)
         matrix)
        (else (append (list (makeTableAux x y M N matrix))(makeTable (+ x 1)y M N matrix)))))


(define (makeTableAux x y M N matrix)
  (cond ((equal? y M)
         matrix)
        (else (append (list 0) (makeTableAux x (+ y 1) M N matrix)))))


;Definiciones para agregar la ficha en el tablero
(define (putM x y matrix num)
  (cond ((equal? x 0)
         (append (list(putM_aux y (car matrix) num))(cdr matrix)))
        (else (append (list(car matrix))(putM (- x 1) y (cdr matrix) num)))))

(define (putM_aux y matrix num)
  (cond ((equal? y 0)
         (cons num (cdr matrix)))
        (else (cons (car matrix)(putM_aux (- y 1) (cdr matrix) num)))))


;Definiciones que verifican si el elemento se encuentra en el tablero
;Verifica la existencia del elemento en el tablero.
(define (hasElementAlready x y matrix num)
  (cond ((equal? x 0)
         (hasElementAlready_aux y (car matrix) num))
        (else (hasElementAlready (- x 1) y (cdr matrix) num))))

(define (hasElementAlready_aux y matrix num)
  (cond ((equal? y 0)
         (equal? num (car matrix)))
        (else (hasElementAlready_aux (- y 1) (cdr matrix) num))))

;Creación de canvas que maneja eventos
(define (TTT M N)
  (cond ((or (or (< M 3) (> M 10)) (or (< N 3) (> N 10)))
         "Tamaño mínimo de 3 y máximo de 10")
        (else( let()
                (set! frame_GUI (new frame%[label "Tic Tac Toe Game"]
                  [stretchable-height #f]
                  [stretchable-width #f]))
                (set! mp M)
                (set! np N)
                (set! gameBoard (makeTable 0 0 M N '())) 
                (cond ((> M N)(calcularSqr M))
                      (else(calcularSqr N)))
                (send frame_GUI min-width(* sqrSize M))
                (send frame_GUI min-height(* sqrSize N))
                (define frame_Canvas(new frame% [parent frame_Canvas]
                                       [paint-callback
                                        (lambda (canvas dc)
                                          (drawColumn M N 0 0 dc))]))
                (send frame_Canvas show #t)))))

