;Mark Ortega-Ponce
;lightsout-game.lisp

;Purpose: To play as a regular player
; Start-game: get initial input for board
; Refresh-game: controller of game after 1st 20 moves
; Get-position-input: where moves takes place, new board made

(load "toggle-functions.lisp")
(load "helper-functions.lisp")

(defun start-game ()
	(let((user-input (get-input "Choose board size 3x3 or 5x5, enter 3 or 5: ")))
	(let((temp-game (create-board-game user-input 0 0)))
	(let((board-game
	    	(if (all-lights-out temp-game);blanking on the chances of this, but I think feedback was 1/32_000_000
	        	(create-board-game user-input 0 0);make board again if all lights out 1/32_000_000 chance
				temp-game)));changes of being empty low, 1/32_000_000 * 1/32_000_00 1/trillion

	(print-board board-game (if (= (mod (length board-game) 3) 0) 3 5))
	(let ((refresh-game-list 
		(get-position-input board-game 
			(if (= (mod (length board-game) 3) 0) ;choose prompt based on 3x3 or 5x5
				"~%Enter position (0-8): " 
				"~%Enter position (0-24): ") 1))
			 )
			 (refresh-game refresh-game-list)      ;call this once past 20 moves reached
		
	);end of 4th let
	);end of 3rd let
	);end of 2nd let
	);end of 1sr let
);;end of start-game

;;once we go past 20 moves, board settings passed as a list from start-game
;;we check if we got a list, if not then that means we won
;;else create a new variable to store future board settings
;;keep looping through this until we win
(defun refresh-game (board-settings)

	(format T "~%Refreshed game, board will be the same. Current Moves Made: ~a" 
		(car(cdr(cdr board-settings))))

	(if (listp board-settings)
		;;car board-settings gives us the board
		;;car cdr gives us the prompt
		;;car cdr cdr gives us the count=
		(let(( new-settings (get-position-input 
							 (car board-settings) 
							 (car(cdr board-settings)) 
							 (+ (car(cdr(cdr board-settings))) 1)))
			)
			(if (listp new-settings)
				(refresh-game new-settings);;create a lopp within refresh-game, need new board to keep looping
				(format T "~% Congrats on winning!")
			)
		)

	)
)

;after receiving our created board in (start-game) function, create a refresh-game-list variable
;assign the board returned from this function to refresh-game-list after 20 moves to prevent 
;stack overflow
;Stack overflow happens because when user hasnt won, we ask for more input and consequently 
;go deeper into recursion
;once we go past 20 moves, control goes from start-game to (refresh-game) function.
;We "save our settings" by putting our parameters into a list and unpacking this list in refresh game
(defun get-position-input (board prompt move-count)
	(format T prompt)
	(let(( position-input (read))
		)
		(let(( new-board ;used to pass board-size with different adjacent function
				(toggle-adjacent board position-input)) 
			)
			(print-board new-board (if (= (mod (length board) 3) 0) 3 5))

			(if (all-lights-out new-board)
				(format T "~%You've won the game after: ~a moves! ~%" move-count)
				(if (= (mod move-count 20) 0)
					;;exit out of recursion to reclaim some stack space, save board settings, put in list
					(list (toggle-adjacent board position-input) prompt move-count) 
					(get-position-input new-board prompt (+ move-count 1))
				)
			)
		);end second let
	);end first let
);end function 