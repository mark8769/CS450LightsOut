;Mark Ortega-Ponce
;helper-functions.lisp

;Purpose: Contains all the helper functions for running the game
;eg. get initial board size input
;create board game with random lights on
;print board after new input
;function for checking if game is won

(defun get-input (prompt)
  (format T prompt)
  (let ( (user-input (read)))
    (if (or (= user-input 3) (= user-input 5) )
    	user-input
      (get-input "~%Number must be 3 or 5! Enter again: ")
    )
  )
)
;after receiving input, we hand back to (start-game function) then call created board-game
(defun create-board-game (board-size counter num-lights-on)

	(if (or (and (= board-size 3) (not (> counter 8)))   ;determine valid input 
					(and (= board-size 5) (not (> counter 24))))
		
		(if (and (oddp (random 100)) (not (= num-lights-on 3)))
			(cons 1 (create-board-game board-size (+ counter 1) (+ num-lights-on 1)))
			(cons 0 (create-board-game board-size (+ counter 1) num-lights-on))
		)
		nil
	)
)

(defun print-board (board row-size)
	;(format T "~%Row size: ~a" row-size)
 	(if (= 0 (mod (length board) row-size))  ;;if current length of list mod (3 or 5)        
		(format t "~% ~a" (car board) )        ;;print a newline character
		(format t " ~a" (car board) )          ;;else just print current element in the list
	)
	(if (null (rest board))                  ;;if no more elements after current element
		nil                                    ;;return nil
		(print-board (rest board) row-size)    ;;keep printing until done
	)
)

;;test of game is done. if true then print out game Won!
(defun all-lights-out (alist)                 
	(if (null alist)                       ;;if no more elements
		T                                    ;;means there are no zeroes because we got to end
		(if (zerop (car alist))              ;;else, keep checking
			(all-lights-out (cdr alist) )      ;;if its zero, keep checking rest of list recursively
			nil                                ;;else return nil because we found a 1
		)
	)
)


;TEST FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Parameters: known board, known-list, move-count

;this function can take input as a list vs. other only user input

;;still follows same format just some modifications
(defun get-position-input-tester (board test-input move-count)
	(if (null test-input)
		(if (not (all-lights-out board))
			(format T "You failed the test")
		)
		(let (( board-size (if (zerop (mod (length board) 3)) 3 5) )
		   	)
		   	(let (( new-board (toggle-adjacent board  (car test-input)))
		   	     )
		   	(if (all-lights-out new-board)
		   		;format nil, indicates to return this as a string, rather than print out as side effect
		   		(format nil "You've won the game after: ~a moves! ~%" (+ move-count 1))
		   		(get-position-input-tester new-board (cdr test-input) (+ move-count 1))
		   	)
		    )
		)
  )
)

(defun run-tests ()
	(format T "~%Print-board tests, check if match: ")
	(format T "~%~%Toggling 2nd row only")
	(print-board '(0 0 0 1 1 1 0 0 0) 3)
	(format T "~%~%Toggling 1st row and last row only")
	(print-board '(1 1 1 0 0 0 1 1 1) 3)
	(format T "~%~%Toggling 1st, and 4th row only")
	(print-board '(1 1 1 1 1 
		             0 0 0 0 0
		             0 0 0 0 0
		             1 1 1 1 1
		             0 0 0 0 0) 5)
	(format T "~%~%Toggling 2nd, 3rd, 5th row")
	(print-board '(0 0 0 0 0
		             1 1 1 1 1
		             1 1 1 1 1
		             0 0 0 0 0
		             1 1 1 1 1) 5)

	(format T "~%~%nil to pass all-lights-out test 3x3: ~a ~%" 
		(all-lights-out '(0 0 0 0 0 0 1 0 0) ) ) ;;test value at beginning of row 3
	(format T "nil to pass all-lights-out test 3x3: ~a ~%" 
		(all-lights-out '(0 0 1 1 0 0 0 0 0) ) ) ;;test value at end of row 1
	(format T "nil to pass all-lights-out test 3x3: ~a ~%" 
		(all-lights-out '(1 0 0 0 0 0 0 0 0) ) ) ;;test value at starting value
	(format T "nil to pass all-lights-out test 3x3: ~a ~%" 
		(all-lights-out '(0 0 0 0 0 0 0 0 1) ) ) ;;test value at end 
	(format T "True to pass all-lights-out test 3x3: ~a ~%" 
		(all-lights-out '(0 0 0 0 0 0 0 0 0) ) ) ;;test with no zeroes
	(format T "nil to pass all-lights-out test 5x5: ~a ~%" 
		(all-lights-out '(0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "nil to pass all-lights-out test 5x5: ~a ~%" 
		(all-lights-out '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "nil to pass all-lights-out test 5x5: ~a ~%" 
		(all-lights-out '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
	(format T "nil to pass all-lights-out test 5x5: ~a ~%" 
		(all-lights-out '(0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "nil to pass all-lights-out test 5x5: ~a ~%" 
		(all-lights-out '(0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "nil to pass all-lights-out test 5x5: ~a ~%" 
		(all-lights-out '(0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0)))
	(format T "nil to pass all-lights-out test 5x5: ~a ~%" 
		(all-lights-out '(0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0)))
	(format T "True to pass all-lights-out test 5x5: ~a ~%" 
		(all-lights-out '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

	;;test all positions in 3x3 matrix, with known values
	(format T "~%Testing 3x3 lights out grid: toggle each light and see if working correctly~%~%")
	(format T "toggle-adjacent test 0: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0)  0) '(1 1 0 1 0 0 0 0 0) )) 
	(format T "toggle-adjacent test 1: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0)  1) '(1 1 1 0 1 0 0 0 0) ))
	(format T "toggle-adjacent test 2: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0)  2) '(0 1 1 0 0 1 0 0 0) ))
	(format T "toggle-adjacent test 3: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0)  3) '(1 0 0 1 1 0 1 0 0) ))
	(format T "toggle-adjacent test 4: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0)  4) '(0 1 0 1 1 1 0 1 0) ))
	(format T "toggle-adjacent test 5: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0)  5) '(0 0 1 0 1 1 0 0 1) ))
	(format T "toggle-adjacent test 6: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0)  6) '(0 0 0 1 0 0 1 1 0) ))
	(format T "toggle-adjacent test 7: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0)  7) '(0 0 0 0 1 0 1 1 1) ))
	(format T "toggle-adjacent test 8: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0)  8) '(0 0 0 0 0 1 0 1 1) )) 
	
	;;test all positions in 5x5 matrix, with known values
	(format T "~%Testing 5x5 lights out grid: toggle each light and see if working correctly~%~%")
	(format T "toggle-adjacent test 0: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0) 
														'(1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "toggle-adjacent test 1: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 1) 
														'(1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "toggle-adjacent test 2: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2) 
														'(0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "toggle-adjacent test 3: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 3) 
														'(0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "toggle-adjacent test 4: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 4) 
														'(0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

	(format T "toggle-adjacent test 5: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 5) 
														'(1 0 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "toggle-adjacent test 6: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 6) 
														'(0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "toggle-adjacent test 7: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 7) 
														'(0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "toggle-adjacent test 8: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 8) 
														'(0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)))
	(format T "toggle-adjacent test 9: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 9) 
														'(0 0 0 0 1 0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0)))

	(format T "toggle-adjacent test 10: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 10) 
														'(0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0)))
	(format T "toggle-adjacent test 11: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 11) 
														'(0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0)))
	(format T "toggle-adjacent test 12: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 12) 
														'(0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0)))
	(format T "toggle-adjacent test 13: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 13) 
														'(0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0) ))
	(format T "toggle-adjacent test 14: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 14) 
														'(0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 1 0 0 0 0 0)))

	(format T "toggle-adjacent test 15: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 15) 
														'(0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 1 0 0 0 0)))
	(format T "toggle-adjacent test 16: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 16) 
														'(0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0)))
	(format T "toggle-adjacent test 17: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 17) 
														'(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0)))
	(format T "toggle-adjacent test 18: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 18) 
														'(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0)))
	(format T "toggle-adjacent test 19: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 19) 
														'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 1)))

	(format T "toggle-adjacent test 20: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 20) 
														'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 1 0 0 0)))
	(format T "toggle-adjacent test 21: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 21) 
														'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0)))
	(format T "toggle-adjacent test 22: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 22) 
														'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0)))
	(format T "toggle-adjacent test 23: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 23) 
														'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1)))
	(format T "toggle-adjacent test 24: ~a ~%" 
		(equal (toggle-adjacent '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 24) 
														'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1)))

	(format T "Need won game prompt to pass test and count 2: ~a ~%" 
		(get-position-input-tester '(0 0 1 1 1 0 0 0 0) '(0 1) 0))

	(format T "Need won game prompt to pass test and count 5: ~a ~%" 
		(get-position-input-tester '(1 1 0 1 1 1 0 1 0 1 0 1 1 1 0 1 0 1 0 1 1 1 0 1 1) '(0 4 12 20 24) 0))
)

