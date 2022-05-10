#lang racket

(require racket)

;Returns the bigger number of the list
(define (getNumMax listMax num)
  (cond ((null? listMax)
         num)
        ((>= (car listMax)num)
         (getNumMax (cdr listMax)(car listMax)))
        (else
         (getNumMax (cdr listMax) num))))

;Returns the smallest member of the list
(define (getNumMin listMin num)
  (cond ((null? listMin)
         num)
        ((<= (car listMin)num)
         (getNumMin (cdr listMin)(car listMin)))
        (else
         (getNumMin (cdr listMin)num))))

;Returns the desired value found in the position of the matrix
(define (getMatrixValue matrix i j)
  (cond ((null? matrix)#f)
        ((equal? i 0)
         (getMatrixValueAux (car matrix) j))
        (else
         (getMatrixValue (cdr matrix) (- i 1) j))))

;Travels the columns of the matrix searching for the desired value
(define (getMatrixValueAux matrix j)
  (cond ((null? matrix) #f)
        ((equal? j 0)
         (car matrix))
        (else (getMatrixValueAux (cdr matrix) (- j 1)))))

;Function that appends a new value and returns the matrix
(define(appendValueMatrix matrix i j newValue)
  (cond ((null? matrix)
         '())
        ((equal? i 0)
         (append (list(appendValueMatrixAux(car matrix) j newValue))(cdr matrix)))
        (else
         (append(list(car matrix))(appendValueMatrixAux(cdr matrix)(- i 1) j newValue)))))

;Recursive Function that appends a new value and returns the matrix
(define (appendValueMatrixAux matrix j newValue)
  (cond ((null? matrix)
         '())
        ((equal? j 0)
         (cons newValue (cdr matrix)))
        (else
         (append (list(car matrix))(appendValueMatrixAux(cdr matrix)(- j 1) newValue)))))

;Empty spaces are represented as a 0, the symbol X is used for the players represented as 1 on the matrix,
;and O is represented for the machine using -1 on the matrix.
(define (setCandidates matrix i j M N num)
  (cond ((equal? i M)
         '())
        (else (append(setCandidatesAux (car matrix) i j M N num)
                     (setCandidates (cdr matrix)(+ i 1) j M N num)))))


;Auxiliar function for candidateSet
(define (setCandidatesAux matrix i j M N num)
  (cond ((equal? j N)
         '())
        (else
         (cond ((equal? (car matrix) num)
                (cons (list i j)(setCandidatesAux (cdr matrix) i (+ j 1) M N num)))
               (else
                (setCandidatesAux (cdr matrix) i (+ j 1) M N num))))))

; Verifies how many moves are left to do
(define (movesLeft matrix M N)
  (cond ((equal? (setCandidates matrix 0 0 M N 0) '())
         #f)
        (else #t)))

#| In the following section you will find the fuctions responsible for the checking if
 # the player or the machine, won or draw. We should corroborate the matrix vertically,
 # horizontaly and diagonally to check if we have a win, or a draw. The methods shown
 # displayed  below will be executed every turn to confirm if someone won, we have a
 # draw or the game continues
|#
        
; Returns true or false depending on whether or not there's a horizontal win
(define (checkHorizontal matrix M N I J num)
  (cond ((equal? I M)#f) 
        ((equal? J N)#f)
        ((and(equal?(getMatrixValue matrix I J) num)
             (checkHorizontalAux matrix M N I (+ J 1) num))
         #t)
        (else (checkHorizontal matrix M N (+ I 1) J num))))

; Auxiliary function for the checkHorizontal function
(define (checkHorizontalAux matrix M N I J num)
  (cond ((equal? I M)#f)
        ((equal? J N)#t)
        ((and
          (equal?(getMatrixValue matrix I J) num)
          (checkHorizontalAux matrix M N I (+ J 1) num))
         #t)
        (else #f)))  

; Returns true or false depending on whether or not there's a vertical win
(define (checkVertical matrix M N I J num)
  (cond ((equal? J N)#f)
        ((equal? I M)#f)
        ((and(equal? (getMatrixValue matrix I J) num)
             (checkVerticalAux matrix M N (+ I 1) J num))#t)
        
        (else
         (checkVertical matrix M N 0 (+ J 1) num))))

; Auxiliary function for the checkVertical function
(define (checkVerticalAux matrix M N I J num)
  (cond ((equal? J N)#f)
        ((equal? I M)#t)
        ((and(equal? (getMatrixValue matrix I J) num)
             (checkVerticalAux matrix M N (+ I 1) J num))#t)
        (else #f)))

; Return true or false depending on whether
(define (checkDiagonal matrix M N num)
  (cond ((checkFirstDiagonal  matrix M N 0 0 num) #t)
        ((checkSecondDiagonal  matrix M N 0 0 num) #t)
        (else #f)))

; Auxiliary function for the checkVertical function that navigates through the matrix
; from left to right checking for existing diagonals and wheather or not they're filled
; with X or O
(define (checkFirstDiagonal matrix M N I J num)
  (cond ((equal? J N) #f)
        ((or (>= (+ I 2) M) (>= (+ J 2) N))
         (checkFirstDiagonal matrix M N 0 (+ J 1) num))
        ((and (equal? (getMatrixValue matrix I J) (getMatrixValue matrix (+ I 1) (+ J 1)))
              (equal? (getMatrixValue matrix (+ I 1) (+ J 1)) (getMatrixValue matrix (+ I 2) (+ J 2)))
              (equal? (getMatrixValue matrix (+ I 2) (+ J 2)) num))
         (cond ((or (>= (+ I 3) M) (>= (+ J 3) N))
                #t)
               (else (checkFirstDiagonal matrix M N (+ I 1) (+ J 1) num))))
        (else (checkFirstDiagonal matrix M N 0 (+ J 1) num))))

; Auxiliary function for the checkVertical function that navigates through the matrix
; from top to bottom checking for existing diagonals and wheather or not they're filled
; with X or O
(define (checkSecondDiagonal matrix M N I J num)
  (cond ((equal? I M) #f)
        ((or (>= (+ I 2) M) (>= (+ J 2) N))
         (checkSecondDiagonal matrix M N (+ I 1) 0 num))
        ((and (equal? (getMatrixValue matrix I J) (getMatrixValue matrix (+ I 1) (+ J 1)))
              (equal? (getMatrixValue matrix (+ I 1) (+ J 1)) (getMatrixValue matrix (+ I 2) (+ J 2)))
              (equal? (getMatrixValue matrix (+ I 2) (+ J 2)) num))
         (cond ((or (>= (+ I 3) M) (>= (+ J 3) N))
                #t)
               (else (checkSecondDiagonal matrix M N (+ I 1) (+ J 1) num))))
        (else (checkSecondDiagonal matrix M N (+ I 1) 0 num))))


; This funtion will return 10 if the player has won, -10 if the computer won or 0
; there no winner yet

(define (getState matrix M N)
  (cond ((checkVertical matrix M N 0 0 1)
         10) ; Check if the player won vertically
        ((checkVertical matrix M N 0 0 -1)
         -10) ; Check if the machine won vertically
        
        ((checkHorizontal matrix M N 0 0 1)
         10) ; Check if the player won horizontally
        ((checkHorizontal matrix M N 0 0 -1)
         -10) ; Check if the machine won horizontally
        
        ((checkDiagonal matrix M N 1)
         10); Check if the player won diagonally
        ((checkDiagonal matrix M N -1)
         -10); Check if the machine won diagonally
        
        (else 0))) ; There's no winner yet