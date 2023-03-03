;Mark Ortega-Ponce
;toggle-functions.lisp

(defun toggle (board pos)
  (if (= 0 pos)                 ; decrement position with recursive call, until 0 and toggle
      (if (zerop (car board))   ; reach this block once we reached toggle target
          (cons 1 (cdr board))  ; if its zero, toggle to 1 and combine back with rest of list
          (cons 0 (cdr board))  ; else it was 1, toggle to 0 and combine back with rest of list
      )
      ;havent reached target position, decrement and recursion
      (cons (car board) (toggle (cdr board) (- pos 1))) 
  )
)

(defun toggle-adjacent (board pos)
	;;try toggling every direction
	(toggle-bottom (toggle-top (toggle-right (toggle-left (toggle-self board pos) pos) pos) pos) pos) 
)

(defun toggle-self (board pos) (toggle board pos))  ;;always toggle self

;;if less than board length than 0, else calculate curr.row
;;if curr. pos = 0 than well fall off left edge so set to -1
;;if curr. pos = 1 then set to 0 to prevent arithmetic error
;;all other cases, make sure future row not < current row
;;else we fell off edge and toggled incorrect position
;;return board if we cant toggle

(defun toggle-left (board pos)
	(let ((board-size (if (= (mod (length board) 3) 0) 3 5)))  ;;determine board size
		(let ((current-row (if (< pos board-size) 0 (truncate (/ pos board-size))))                 
					(future-row (if (= pos 0) -1 (if (= pos 1) 0 (truncate (/ (- pos 1) board-size))))))  
					(if (< future-row current-row)                                                        
						board                                                                               
						(toggle board (- pos 1))                                                            
					)                            
		)
	)
)

;determine current row if < board-length than 0, else calculate
;calculate future row if we were to move value
;can't toggle if it falls of the right edge onto next row
;toggling only works if in same row, so check for this in the if
;if it doesnt "fall" off edge than toggle
;else return the board with no changes

(defun toggle-right (board pos)                                                     
	(let ((board-size (if (= (mod (length board) 3) 0) 3 5))) ;determine board size     
		(let ((current-row (if (< pos board-size) 0 (truncate (/ pos board-size))))   
					(future-row (truncate (/ (+ pos 1) board-size)))                           
					(index-limiter (if (= board-size 3) 8 24)))                                 
					(if (and (not (> future-row current-row)) (not (> (+ pos 1) index-limiter)))  
						(toggle board (+ pos 1))                                                    
						board                                                                       
					)
		)
	)
)

;toggle top value if it doesnt go out of range, eg. 0
;toggling pos 1, we cant toggle (1 - 5) for 5x5 board
;else return board

(defun toggle-top (board pos)
	(let ((board-size (if (= (mod (length board) 3) 0) 3 5))) ;determine board size
		(let ()                                                      
				(if (not (< (- pos board-size) 0))   
					(toggle board (- pos board-size))    
					board)))                             
)

;index-limiter is to prevent toggling something not in scope of board-game eg. 8 or 24
;only toggle if ( position-input + (3 or 5) ) not greater than 8 or 24 
;return board if we can't toggle it

(defun toggle-bottom (board pos)
	(let ((board-size (if (= (mod (length board) 3) 0) 3 5))) ;determine board size were using
		(let ((index-limiter (if (= board-size 3) 8 24)))        
				(if (not (> (+ pos board-size) index-limiter))       
					(toggle board (+ pos board-size))                  
					board                                              
				)
		)
	)
)
