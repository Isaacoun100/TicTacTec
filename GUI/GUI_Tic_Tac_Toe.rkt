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
(define red-marker (make-object pen% "RED" 4 'solid'))
(define blue-marker (make-object pen% "BLUE" 4 'solid'))
(define black-marker (make-object pen% "BLACK" 4 'solid'))

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
             (set! py(aproxPos(send event get-y)))
             (cond ((or(hasAlready py px gameBoard 1) (hasAlready py px gameBoard -1))
                    (displayIn gameBoard)
                    ;Verificar condiciones de gane, perdida o empate
                    )))))
    (super-new)))

;Para dibujar en el gameBoard

(define (drawColumn fila columna cantidadLineas cantidadColumnas dc)
  (send dc set-pen black-pen)
  (cond ((equal? columna cantidadColumnas))
        (else (let ()
                (drawLine fila columna cantidadLineas cantidadColumnas dc)
                (send dc draw-rectangle(* cantidadLineas sqrSize)(* cantidadColumnas sqrSize)sqrSize sqrSize)
                (drawColumn fila columna cantidadLineas(+ 1 cantidadColumnas)dc)))))

(define (drawLine))


