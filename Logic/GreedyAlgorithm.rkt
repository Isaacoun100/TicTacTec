#lang racket

(require racket)
(provide (all-defined-out))

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
         (append
          (list(car matrix))
          (appendValueMatrix
           (cdr matrix)
           (- i 1)
           j
           newValue)))))

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

#| Moveset selection, in this section the computer calculates the best possible moveset to counter
 # what the user is trying to do and also try to win, the logic will be used to first of all not
 # lose the match and second seek for a wim
|#

; This function recieves the matrix, the dimensions and the value we're looking for (-1 for the
; computer, and 1 for the player) then it calculates the best possible move based on the greedy
; algorithm
(define (getBestMove matrix M N)
  (getBestMoveAux matrix -1000 M N 0 0 -1 -1))

; This function will return the position i j where the greedy algorithm analized and concluded
; that is the best posible move in the given position using this algorithm
;
(define (getBestMoveAux matrix bestValue M N I J X Y) 
  (cond ((equal? I M)
         (list X Y))
        ((equal? J N)
         (getBestMoveAux matrix bestValue M N (+ I 1) 0 X Y))
        ((equal? (getMatrixValue matrix I J) 0)
         (cond ((> (minMax (appendValueMatrix matrix I J 1)
                          0 #f M N 0 0 (getState matrix M N)) bestValue)
                (getBestMoveAux matrix
                                 (minMax (appendValueMatrix matrix I J 1)
                                          0
                                          #f
                                          M
                                          N
                                          0
                                          0
                                          (getState matrix M N))
                                 M N I (+ J 1) I J))
               (else (getBestMoveAux matrix bestValue M N I (+ J 1) X Y))))
         (else
          (getBestMoveAux matrix bestValue M N I (+ J 1) X Y))))

; The minMax fuction recieves a list of the posible moves, a matrix, the depth of the
; moves, a boolean value depending on whether or not the value is the max possible,a
; variable M that corresponds to the i value in the matrix and a N that corresponds to
; the j, the variables i and j are the actual size of the matrix and score is the value
; that is used to sort the posibles moves

(define (minMax matrix depth isMax M N I J score)
  (cond ((equal? depth 3)
         score)
        ((equal? score 10)
         score)
        ((equal? score -10)
         score)
        ((equal? (movesLeft matrix M N) #f)
         0)
        (isMax
         (Maxer matrix -1000 M N 0 0 isMax depth))
        (else
         (minimizer matrix 1000 M N 0 0 isMax depth))))

; Looks for the value with the best score in the given depth
(define (Maxer matrix best M N I J isMax depth)
  (cond ((equal? I M)
         best)
        ((equal? J N)
         (Maxer matrix best M N (+ I 1) 0 isMax depth))                                          
        ((equal?(getMatrixValue matrix I J) 0)
         (Maxer matrix
                    (max best (minMax (appendValueMatrix matrix I J 1)
                             (+ depth 1) (not isMax) M N 0 0
                             (getState
                              (appendValueMatrix matrix I J 1)
                              M N))) M N I (+ J 1) isMax depth))
        (else
         (Maxer matrix best
                    M N I (+ J 1) isMax depth))))

; Looks for the value with the best score in the given depth
(define (minimizer matrix best M N I J isMax depth) 
  (cond ((equal? I M)
         best)
        ((equal? J N)
         (minimizer matrix best M N (+ I 1) 0 isMax depth))
        ((equal?(getMatrixValue matrix I J) 0)
         (minimizer matrix
                    (min best(minMax (appendValueMatrix matrix I J -1)
                             (+ depth 1) (not isMax) M N 0 0
                             (getState (appendValueMatrix matrix I J -1) M N)))
                    M N I (+ J 1) isMax depth))
        (else
         (minimizer matrix best
                    M N I (+ J 1) isMax depth))))

; This line of code was a solution fix found in the racket documentation
; https://docs.racket-lang.org/guide/module-provide.html and later on
; was studied using the examples in this stackoverflow forum
; https://stackoverflow.com/questions/54890240/how-to-provide-all-functions-associated-with-a-struct-in-racket

(provide getBestMove setCandidates getState)

#| In the following section you will find the fuctions responsible for the checking if
 # the player or the machine, won or draw. We should corroborate the matrix vertically,
 # horizontaly and diagonally to check if we have a win, or a draw. The methods shown
 # displayed  below will be executed every turn to confirm if someone won, we have a
 # draw or the game continues
|#

; Returns true or false depending on whether or not there's a horizontal win
; Checks if someone won on the different rows of the game 
; i = number rows
; j = number columns
; A = 0 and B = 0 (both start at 0)
; cont = 0 (if cont = 3, means a winner was found)
(define (checkHorizontal matrix i j A B M cont)
  (cond ((> A i)
        #f)
        ((equal? cont 3)
         #t)
        ((>= (+ B 1) j)
         (checkHorizontal matrix i j (+ A 1) 0 M 0))
        ((equal? (getMatrixValue matrix A B) M)
         (checkHorizontal matrix i j A (+ B 1) M (+ cont 1)))
        (else
         (checkHorizontal matrix i j A (+ B 1) M 0))
        ))


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

; Recursively navigates through the matrix looking for
; provable wins in the diagonals
(define (checkDiagonal matrix i j A B M)
  (cond ((and (< i A))
         #f)
        ((and (>= i (+ B 2))(>= j (+ A 2))
              (equal? (getMatrixValue matrix A B) M)
              (equal? (getMatrixValue matrix (+ A 1) (+ B 1)) M)
              (equal? (getMatrixValue matrix (+ A 2) (+ B 2)) M))
         #t)
        (else
         (checkDiagonalAux matrix i j A B M))))

(define (checkDiagonalAux matrix i j A B M)
  (cond ((= B j)
         (checkDiagonal matrix i j (+ A 1) 0 M))
        (else (checkDiagonal matrix i j A (+ B 1) M))))



; This funtion will return 10 if the player has won, -10 if the computer won or 0
; there no winner yet

(define (getState matrix M N)
  (cond ((checkVertical matrix M N 0 0 1)
         10) ; Check if the player won vertically
        ((checkVertical matrix M N 0 0 -1)
         -10) ; Check if the machine won vertically

        ((checkHorizontal matrix M N 0 0 1 0)
         10) ; Check if the player won horizontally
        ((checkHorizontal matrix M N 0 0 -1 0)
         -10) ; Check if the machine won horizontally

        ((checkDiagonal matrix M N 0 0 1)
         10); Check if the player won diagonally
        ((checkDiagonal matrix M N 0 0 -1)
         -10); Check if the machine won diagonally

        (else 0))) ; There's no winner yet