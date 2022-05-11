#lang racket

(require racket/gui)

; Access to the file that contains the logic of the game
(require "../Logic/GreedyAlgorithm.rkt")

; Developers:
; Michael Valverde Navarro
; Isaac Herrera Monge
; Naheem Johnson Solís

; This are the variables that are used to declare the frame that is used in the
; GUI to play the ti tac toe game

; Defines the frame as a clean frame with one canvas
(define frame null)

; Determinates the width of the frame that is drawn
(define width 600)

; Determinates the height of the frame that is drawn
(define height 600)

; Makes the canvas and frame both squared
(define sqrSize 0)

; Defines the X sixe of the canvas in the frame
(define pX 0)

; Defines the Y size of the canvas in the frame
(define pY 0)

; Determinates the width of the canvas relative to the  frame
(define mp 0)

; Determinates the height  of the canvas relative to the  frame
(define np 0)

; This is the list that contains the matrix that is sent
(define aiList '())

; Board of the current game state (This is the )
(define gameBoard '())

; Assings a color to the pen that is being used, in this case red-pen is
; assigned to the color RED (#FF0000)
(define red-pen (make-object pen% "RED" 4 'solid))

; This is the pen associated to the blue color (#0000FF)
(define blue-pen (make-object pen% "BLUE" 4 'solid))

; This is the pen associated to the black color (#000000)
(define black-pen (make-object pen% "BLACK" 2 'solid))

; Makes the canvas a bitmap
(define x-symbol(make-object bitmap% "x.png"))

(define (calcSqr max)
  (set! sqrSize (/ 600 max)))
(define (aproxPos x)
  (floor ( / x sqrSize)))

; gameCanvas is a canvas that manages the events, this means that this function
; is the responsible for receiving the input from  user and writes it into
; the gameBoard
(define gameCanvas%
  (class canvas% ; Here we indicate how much of the window is the canvas
    ; Mouse input handling events are managed here
    (define/override (on-event event)

      ;Activates when the user presses the click button
      (cond ((send event button-down? 'left)

             ; Receives the X position of the mouse in the exact moment the
             ; mouse click is pressed
             (set! pX(aproxPos(send event get-x)))

             ; Receives the Y position of the mouse in the exact moment the
             ; mouse click is pressed
             (set! pY(aproxPos(send event get-y)))

             ; If the user presses an XY value that corresponds to a used square
             ; where there is already a value then the program will identify
             ; this as an error and will display an error message and will let
             ; the user choose another option
             (cond ((or (hasAlredy pY pX gameBoard 1)
                        (hasAlredy pY pX gameBoard -1))

                    ; Error message shown if the user presses the wrong square
                    (displayln "Please press an empty square"))
                   (else

                    ; If there is no error with the position it will compute the
                    ; given X and Y coordinates and then assign it to a value in
                    ; the matrix
                    (set! gameBoard(putM pY pX gameBoard 1))
                    (displayln gameBoard)


                    ; This function runs every time there is a new turn, its job
                    ; is to identify if the computer won, if the player won, if
                    ; there's a draw or if nothing has happened
                    (cond((equal?(getState gameBoard Np Mp)10)

                          ; It will dispkay a finnishing message in the console
                          ; saying "The player is the winner" if the algorithm
                          ; determinates that the player won
                          (displayln "The player is the winner")
                          (send frame show #f))

                         ((equal?(setCandidates gameBoard 0 0 Np Mp 0)'())

                          ; If on the other hand, the algorithm has determinated
                          ; that the game ended in a draw, then, it will display
                          ; a message saying "Is a draw!, Thanks for playing"
                          (displayln "Is a draw!, Thanks for playing")
                          (send frame show #f))
                         (else

                          ; If the player hasn't won, then, it will send the
                          ; current matrix to the getBestMove function to find
                          ; the next best move to play
                          (drawEle pX pY (send this get-dc) 0)
                          (set! aiList (getBestMove gameBoard Np Mp))
                          (drawEle (cadr aiList) (car aiList)
                                   (send this get-dc) 1)
                          (set! gameBoard(putM (car aiList) (cadr aiList) gameBoard -1))
                          (displayln gameBoard)

                          ;If after the move it finds that the computer is the
                          ; winner, then it will display the message "You lose,
                          ; better luck next time"
                          (cond((equal?(getState gameBoard Np Mp)-10)
                                (displayln "You lose, better luck next time")
                                (send frame show #f))

                                ; If after the machine plays there is no winner
                                ; and it finds that there's no point in keep
                                ; playing, then it will display a message saying
                               ((equal?(setCandidates gameBoard 0 0 Np Mp 0)'())
                                (displayln "Is a draw!, Thanks for playing")
                                (send frame show #f)
                                ))))))))) (super-new)))

; Draws the vertical rows of the game board where we will be playig using the
; black-pen
(define (drawColumn row column lineCount columnCount dc)
  (send dc set-pen black-pen)
  (cond((equal? column columnCount))
       (else (let()
               (drawLine row column lineCount columnCount dc)
               (send dc draw-rectangle(* lineCount sqrSize)(* columnCount sqrSize)sqrSize sqrSize)
               (drawColumn row column lineCount (+ 1 columnCount) dc)))))

; Draws the horizontal columns of the game board where we will be playig using the
; black-pen
(define (drawLine row column lineCount columnCount dc)
  (cond((equal? row lineCount))
       (else (let()
              (send dc draw-rectangle(* lineCount sqrSize)(* columnCount sqrSize)sqrSize sqrSize)
              (drawLine row column (+ 1 lineCount) columnCount dc)))))

; This function recieves an X and Y position and draws a X using the red-pen
(define (drawX x y dc)
  (send dc set-pen red-pen)
  (send dc draw-line x y (-(+ x sqrSize)10) (-(+ y sqrSize)10))
  (send dc draw-line x (-(+ y sqrSize)10) (-(+ x sqrSize)10) y))

; This function recieves an X and Y position and draws a O using the blue-pen
(define (drawO x y dc)
  (send dc set-pen blue-pen)
  (send dc draw-ellipse x y (- sqrSize 10) (- sqrSize 10)))

; This function is used to redraw the canvas when there is an new answer from
; the GreedyAlgorithm, it will read the matrix and display it
(define (drawEle pX pY dc type)
  (cond ((equal? type 0)
         (drawX (+ (* sqrSize pX) 5)(+ (* sqrSize pY) 5)dc))
        ((equal? type 1)
         (drawO (+ (* sqrSize pX) 5)(+ (* sqrSize pY) 5)dc))))

; Renders the board where the Tic Tac Toe will be played, it renders the y lines
(define (makeTable x y M N matrix)
  (cond ((equal? x N)
         matrix)
        (else (append (list (makeTableAux x y M N matrix))(makeTable (+ x 1)y M N matrix)))))

; Auxiliarty fucntion for the makeTable function, it renders the y lines
(define (makeTableAux x y M N matrix)
  (cond ((equal? y M)
         matrix)
        (else (append (list 0) (makeTableAux x (+ y 1) M N matrix)))))

; Receives a matrix, an number and X and Y coordinates, using this info it marks
; a X or a O depending on the num value (1 for X and -1 for O)
(define (putM x y matrix num)
  (cond ((equal? x 0)
         (append (list(putM_aux y (car matrix) num))(cdr matrix)))
        (else (append (list(car matrix))(putM (- x 1) y (cdr matrix) num)))))

; Auxiliarty function for the putM fucntion
(define (putM_aux y matrix num)
  (cond ((equal? y 0)
         (cons num (cdr matrix))).
        (else (cons (car matrix)(putM_aux (- y 1) (cdr matrix) num)))))

; When checking if the user has pressed twice the same tile, this method will be
; called, it recieves the x and y coordinates, a matrix and a num, that
; corresponds to the value that we're trying to put in, if there is already a
; value in the give X and Y position of the matrix, then it will return an error
(define (hasAlredy x y matrix num)
  (cond ((equal? x 0)
         (hasAlredy_aux y (car matrix) num))
        (else (hasAlredy (- x 1) y (cdr matrix) num))))

; Auxiliarty function for hasAlredy function
(define (hasAlredy_aux y matrix num)
  (cond ((equal? y 0)
         (equal? num (car matrix)))
        (else (hasAlredy_aux (- y 1) (cdr matrix) num))))

; This is the method that is called to initialize the game
; TTT recieves two parammeters, M and N, which corresponds to the coordinates
; that the tic tac toe matrix will have, it can go from 3x3 to 10x10
(define (TTT M N)

  ; Restricts the user from inputing any other matrix value different from 3x3
  ; or 10x10
  (cond ((or (or (< M 3) (> M 10)) (or (< N 3) (> N 10)))
         "The game only accepts 3x3 to 10x10 matrixes")
        (else( let()
                (set! frame (new frame%[label "TIC TAC TEC"]
                  [stretchable-height #f]
                  [stretchable-width #f]))
                (set! Mp M)
                (set! Np N)
                (set! gameBoard (makeTable 0 0 M N '()))
                (cond ((> M N)(calcSqr M))
                      (else(calcSqr N)))
                (send frame min-width(* sqrSize M))
                (send frame min-height(* sqrSize N))
                (define gameCanvas(new gameCanvas% [parent frame]
                                       [paint-callback
                                        (lambda (canvas dc)
                                          (drawColumn M N 0 0 dc))]))
                (send frame show #t)))))
