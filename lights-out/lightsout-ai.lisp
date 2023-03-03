;Mark Ortega-Ponce
;lightsout-ai.lisp

;Purpose: Let AI take over

;ai-player: same as start game
;refresh-gaem-ai: recover stack space, controller after 1st 20 moves
;get-ai-position-input: let ai make moves
;ai-input: series of steps, chasing the lights down
;or using winnable configurations from testing
;check-winnable 3x3/5x5 some winnable configurations

(load "toggle-functions.lisp")
(load "helper-functions.lisp")

(defun ai-player ()
	(let((user-input (get-input "Choose board size 3x3 or 5x5, enter 3 or 5: ")))
	(let((temp-game (create-board-game user-input 0 0)))
	(let((board-game
			(if (all-lights-out temp-game)
				(create-board-game user-input 0 0)
				temp-game)))

	(print-board board-game (if (= (mod (length board-game) 3) 0) 3 5))
	(let ((refresh-game-list  ;set to return value of get-ai-position, either board/win prompt
		(get-ai-position-input board-game "~%" 1 -1)))
			;;start loop for game
			(refresh-game-ai refresh-game-list 0)
	);end 4th let
	);end 3rd let
	);end 2nd let
	);end 1st let
)

(defun refresh-game-ai (board-settings counter)
	(if (listp board-settings)
		(let (( new-settings (get-ai-position-input                   ;function call
								(car board-settings)                  ;board
								(car(cdr board-settings))             ;prompt
								(+ (car(cdr(cdr board-settings))) 1)  ;move-count
								(car(cdr(cdr(cdr board-settings)))))) ;previous-input
			   )
			(if (listp new-settings) 
				(format T "~%Refreshed game. Current Moves Made: ~a" (car(cdr(cdr board-settings)))))
			;;change counter to <30,
			;create a loop within refresh-game, need new board to keep looping 
			(if (listp new-settings)
				(if (= counter 20)
					board-settings
					(refresh-game-ai new-settings (+ counter 1)) 
				)
				(format T new-settings) ;;win prompt, if board is not in settings form
			)
		)
		;used for first pass. eg. If less than 20 and won
		;else we use the one inside the let function
		(format T board-settings);;this will be, you won the game prompt from get-ai-pos function
	)
)

(defun get-ai-position-input (board prompt move-count previous-input)
	(format T prompt)
	;(format T "~%Moves: ~a "move-count)
	(let ((board-size (if (= (mod (length board) 3 ) 0 ) 3 5)))
	(let ( ( position-input (ai-input board 0 board-size))     ;take AI input
		   ( random-input (random (* board-size board-size)))  ;if prev-input = curr-input use this
		 )
		(let (( new-board
			(if (= previous-input position-input)
				(toggle-adjacent board random-input)   ;use random input if move repeated
				(toggle-adjacent board position-input) ;;else first pass will be ai input
			)
		));end 3rd let declarations
		;;if random input was changed, print "overwritten". Use for analyzing winning combinations
		(if (= previous-input position-input)
			;;(format T " AI input overwritten: ~a " random-input) 
			;;(format T " AI input: ~a " position-input) 
			(format T " AI input overwritten: ~a | Data: (if (equal board '~a) ~a" random-input board random-input) 
			(format T " AI input: ~a | Data: (if (equal board '~a) ~a" position-input board position-input))

		(print-board new-board board-size)

		(if (all-lights-out new-board)
			;return as a string for refresh game ai, dont want refresh game output
			(format nil "~%You've won the game after: ~a moves! ~%" move-count)
			(if (= (mod move-count 20) 0)
				(list (toggle-adjacent board position-input) prompt move-count position-input)
				(get-ai-position-input new-board prompt (+ move-count 1) position-input)
				;;just do random input, dont pass as previous input, brings average up considerably
				;; if prev = curr pass random as previous input, else position input
				;;(get-ai-position-input new-board prompt (+ move-count 1) random-input)
			)
		)

	);;close 3rd let
	);;close 2nd let
	);;close 1st let
);;close function

;;Using a combination of "Chasing the Lights" down to the bottom row
;;and winnable combinations found while testing it
;;if we find a winnnable combination, then go through those first
;;else we want to chase the lights down and hopefully have one of
;;winnable combinations already in our list
(defun ai-input (board pos-index board-size)
	(let ((winnable (if (= board-size 3) (check-winnable board) (check-winnable-five-by-five board))))
		(if (null board)
			(- pos-index 1)                     ;;return (last-index + 1) - 1, so last index
			(if (not (null winnable))           ;;if winnable combination found, skip rest
				winnable
				(if (not (= (car board) 1)) ;;if lights isnt on keep going down the list
					(ai-input (cdr board) (+ pos-index 1) board-size) ;;make recursive call if light isnt on
					(if (> (+ pos-index board-size) (- (* board-size board-size) 1))
						pos-index
						(+ pos-index board-size);if light on, toggle bottom light under it
					) 
				)
			)
		)
	)
)
;;found through testing of AI function
(defun check-winnable (board)

	(if (equal board '(0 1 0 1 1 1 0 1 0)) 4
	(if (equal board '(1 1 0 1 0 0 0 0 0)) 0
	(if (equal board '(0 1 1 0 0 1 0 0 0)) 2
	(if (equal board '(0 0 0 0 0 1 0 1 1)) 8
	(if (equal board '(0 0 0 1 0 0 1 1 0)) 6
	(if (equal board '(1 0 0 0 1 0 1 0 0)) 3
	(if (equal board '(0 0 0 1 0 0 0 0 0)) 6
	(if (equal board '(0 0 0 0 0 0 1 1 0)) 2
	(if (equal board '(0 1 1 0 0 1 1 1 0)) 4
	(if (equal board '(0 0 1 1 1 0 1 0 0)) 5
	(if (equal board '(0 0 0 1 0 1 1 0 1)) 6
	(if (equal board '(1 0 1 1 0 0 0 0 0)) 3
	(if (equal board '(0 0 1 0 1 0 1 0 0)) 5
	(if (equal board '(0 0 0 0 0 1 1 0 1)) 8
	(if (equal board '(0 1 1 0 0 1 1 1 0)) 4
	(if (equal board '(0 0 1 1 1 0 1 0 0)) 5
	(if (equal board '(0 1 0 1 1 0 1 1 1)) 4
	(if (equal board '(1 1 0 0 0 0 0 1 1)) 3
	(if (equal board '(0 0 0 1 0 0 0 1 1)) 0
	(if (equal board '(1 1 0 1 0 0 1 0 0)) 3
	(if (equal board '(0 1 0 0 1 0 0 0 0)) 4
	(if (equal board '(0 0 0 1 0 1 0 1 0)) 6
	(if (equal board '(0 0 0 0 0 1 1 0 0)) 8
	(if (equal board '(0 0 0 1 0 0 0 0 1)) 6
	(if (equal board '(0 0 1 1 1 1 0 0 0)) 5
	(if (equal board '(0 0 0 1 0 0 0 0 1)) 6
	(if (equal board '(0 0 0 0 0 0 1 1 1)) 6
	(if (equal board '(1 1 1 0 1 0 1 1 1)) 3
	(if (equal board '(1 1 1 0 0 0 0 0 0)) 3
	(if (equal board '(0 1 1 1 1 0 1 0 0)) 4
	(if (equal board '(0 0 1 0 0 1 1 1 0)) 5
	(if (equal board '(0 0 1 0 1 0 0 1 0)) 5
	(if (equal board '(1 1 1 0 0 0 0 0 0)) 3
	(if (equal board '(0 1 1 1 1 0 1 0 0)) 4
	(if (equal board '(0 0 1 0 0 1 1 1 0)) 5
	(if (equal board '(0 0 0 0 0 0 0 0 1)) 1
	(if (equal board '(1 1 1 0 1 0 0 0 1)) 3
	(if (equal board '(0 1 1 1 0 0 1 0 1)) 4
	(if (equal board '(0 0 1 0 1 1 1 1 1)) 5
	(if (equal board '(0 0 0 0 0 0 1 1 0)) 2
	(if (equal board '(0 1 1 0 0 1 1 1 0)) 4
	(if (equal board '(0 0 1 1 1 0 1 0 0)) 5
	(if (equal board '(0 0 0 1 1 1 1 0 1)) 6
	(if (equal board '(0 0 0 0 1 1 0 1 1)) 7
	(if (equal board '(0 0 0 1 0 0 0 0 1)) 3	
	(if (equal board '(1 0 0 0 1 0 1 0 1)) 3
	(if (equal board '(0 0 0 0 0 0 1 1 1)) 1
	(if (equal board '(0 1 1 1 0 0 0 1 1)) 4
	(if (equal board '(0 0 0 0 1 0 1 0 0)) 7	
	(if (equal board '(0 0 0 0 0 0 0 1 1)) 0	
	(if (equal board '(1 1 0 1 0 0 0 1 1)) 3
	(if (equal board '(0 1 0 0 1 0 1 1 1)) 4
	(if (equal board '(0 0 1 0 1 1 0 0 0)) 5

	nil)))))))))))))))))))))))))))))))))))))))))))))))))))))
)


(defun check-winnable-five-by-five (board)

	(if (equal board '(1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 0	
	(if (equal board '(1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 1	
	(if (equal board '(0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 2
	(if (equal board '(0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 3
	(if (equal board '(0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 4
	(if (equal board '(1 0 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 5	
	(if (equal board '(0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0)) 6
	(if (equal board '(0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0)) 7
	(if (equal board '(0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)) 8
	(if (equal board '(0 0 0 0 1 0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0)) 9
	(if (equal board '(0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0)) 10	
	(if (equal board '(0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0)) 11
	(if (equal board '(0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0)) 12
	(if (equal board '(0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0)) 13
	(if (equal board '(0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 1 0 0 0 0 0)) 14
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 1 0 0 0 0)) 15	
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0)) 16
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0)) 17
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 1)) 19
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 1 0 0 0)) 20	
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0)) 21
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0)) 22
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1)) 23
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1)) 24
	(if (equal board '(0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 1 0 0 0 0 1 1 0 1 1)) 10
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1)) 20
	(if (equal board '(0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 1 0 0 0 0 1 1 0 1 1)) 10
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1)) 20
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1)) 8
	(if (equal board '(0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 1 0 0 0 0 0 0 0 1 1)) 8
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1)) 20
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1)) 4
	(if (equal board '(0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1)) 8
	(if (equal board '(0 0 0 0 1 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 1 1 0 1 1)) 9
	(if (equal board '(0 0 0 0 0 0 0 1 0 1 0 0 0 1 1 0 0 0 0 0 1 1 0 1 1)) 12
	(if (equal board '(0 0 0 0 0 0 0 0 0 1 0 1 1 0 1 0 0 1 0 0 1 1 0 1 1)) 14
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 1 0 1 1 1 0 1 1)) 16
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 0 0 1 1 0 0 1 1)) 17
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 1 1 1 0 1 1 1)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 1 0 1)) 20
	(if (equal board '(0 0 0 0 1 0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 1 1 0 1)) 9
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1)) 21
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 1)) 0
	(if (equal board '(1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 1)) 5
	(if (equal board '(0 1 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 1)) 6
	(if (equal board '(0 0 0 0 0 1 0 1 0 0 1 1 0 0 0 0 1 0 0 0 1 0 0 0 1)) 10
	(if (equal board '(0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1 0 0 0 1 0 0 0 1)) 12
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 1 1 0 0 1 0 0 0 1)) 16
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 1 0 0 1)) 17
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1 0 1 1 1 0 1)) 18
	(if (equal board '(0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 1 0 0 0 1 0 1 1 0)) 7
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 1 0)) 21
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 1 1 0 1 0)) 15
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0)) 21
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 1 0)) 1
	(if (equal board '(1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 1 0)) 5
	(if (equal board '(0 1 1 0 0 1 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 1 1 0)) 6
	(if (equal board '(0 0 1 0 0 0 1 1 0 0 1 1 0 0 0 0 1 0 0 0 1 0 1 1 0)) 7
	(if (equal board '(0 0 0 0 0 0 0 0 1 0 1 1 1 0 0 0 1 0 0 0 1 0 1 1 0)) 13
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 0 1 0 1 0 1 0 1 1 0)) 15
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 0 0 1 0 0 0 1 1 0)) 16
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 0 0 1 1 1 0)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 1 0 1 1 0 0)) 19
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 1 1 0 1)) 21
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 1)) 23
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0)) 20
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 1 1 0)) 4
	(if (equal board '(0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 1 1 0)) 8
	(if (equal board '(0 0 0 0 1 0 0 1 1 0 0 0 0 1 0 1 0 0 0 0 0 1 1 1 0)) 9
	(if (equal board '(0 0 0 0 0 0 0 1 0 1 0 0 0 1 1 1 0 0 0 0 0 1 1 1 0)) 12
	(if (equal board '(0 0 0 0 0 0 0 0 0 1 0 1 1 0 1 1 0 1 0 0 0 1 1 1 0)) 14
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 0 1 0 1 0 1 1 1 0)) 16
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 0 0 1 0 0 1 1 0)) 17
	(if (equal board '(0 0 0 0 0 0 1 1 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0)) 11
	(if (equal board '(0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 1 0 0 0 0 0 0 0 0)) 12
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 1 1 0 0 0 0 0 0 0)) 16
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 0 1 0)) 22
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 0 0)) 23
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1)) 24
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0)) 23
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 0 0 1 1 1)) 22
	(if (equal board '(0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 1 1 1 1 1 0 1 0 0 1)) 11
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 0 1 0 0 1)) 20
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 1)) 22
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1)) 23
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0)) 24
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 1 1)) 15
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 1 1 0 0 1)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1 1 0 0 0 1 1 1)) 17
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1)) 11
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1)) 4
	(if (equal board '(0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1)) 8
	(if (equal board '(0 0 0 0 1 0 0 1 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 1 1)) 9
	(if (equal board '(0 0 0 0 0 0 0 1 0 1 0 0 0 1 1 1 0 0 0 0 0 0 0 1 1)) 12
	(if (equal board '(0 0 0 0 0 0 0 0 0 1 0 1 1 0 1 1 0 1 0 0 0 0 0 1 1)) 14
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 0 1 0 1 0 0 0 1 1)) 16
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 0 0 1 0 1 0 1 1)) 17
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 1 1 1 1)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1 1 1 1)) 21
	(if (equal board '(0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 1 0 0)) 14
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1)) 24
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0)) 20
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0)) 1
	(if (equal board '(1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0)) 5
	(if (equal board '(0 1 1 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0)) 6
	(if (equal board '(0 0 1 0 0 0 1 1 0 0 1 1 0 0 0 1 0 0 0 0 0 0 1 0 0)) 7
	(if (equal board '(0 0 0 0 0 0 0 0 1 0 1 1 1 0 0 1 0 0 0 0 0 0 1 0 0)) 13
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 0 0 1 0 0 0 1 0 0)) 15
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 1 0 1 0 1 0 1 0 0)) 16
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 1 0 1 1 1 0 0)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 1 1 1 1 0)) 19
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1 1 1 1 1)) 20
	(if (equal board '(1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 5
	(if (equal board '(0 1 1 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 6
	(if (equal board '(0 0 1 0 0 0 0 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)) 7
	(if (equal board '(0 0 0 0 0 0 1 0 1 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0)) 11
	(if (equal board '(0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0)) 13
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 0 1 0 0 0 0 0 0)) 17
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 1 1 0)) 19
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)) 22
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1)) 1
	(if (equal board '(1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1)) 5
	(if (equal board '(0 1 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 1)) 6
	(if (equal board '(0 0 1 0 0 0 1 1 0 0 1 1 0 0 0 0 0 1 0 0 0 1 0 0 1)) 7
	(if (equal board '(0 0 0 0 0 0 0 0 1 0 1 1 1 0 0 0 0 1 0 0 0 1 0 0 1)) 13
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 0 0 1 1 0 0 1 0 0 1)) 15
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 1 1 1 0 1 1 0 0 1)) 16
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 0 1 0 0 0 1)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1 1 0 0 1 1)) 19
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 0 0 1 0)) 22
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 0 0)) 23
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 1 1 0 0 0 0 1)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 0 1 1 1 1 1)) 17
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1)) 23
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 0 0)) 24
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1)) 20
	(if (equal board '(0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 1)) 6
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1)) 20
	(if (equal board '(0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 1 1 0 1 1)) 8
	(if (equal board '(0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 7
	(if (equal board '(0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0)) 13
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1 0 0 0 1 0)) 19
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 1 1)) 22
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 0 1)) 23
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 1 0)) 2  
	(if (equal board '(0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 1 0)) 6
	(if (equal board '(0 0 1 1 0 1 1 0 0 0 0 1 0 0 0 0 1 0 0 0 1 0 1 1 0)) 7
	(if (equal board '(0 0 0 1 0 1 0 1 1 0 0 1 1 0 0 0 1 0 0 0 1 0 1 1 0)) 8
	(if (equal board '(0 0 0 0 0 1 0 0 0 1 0 1 1 1 0 0 1 0 0 0 1 0 1 1 0)) 10
	(if (equal board '(0 0 0 0 0 0 0 0 0 1 1 0 1 1 0 1 1 0 0 0 1 0 1 1 0)) 14
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 1 1 0 0 1 1 0 1 1 0)) 15
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 1 0 0 1 1 0)) 17
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 1 0 0 0 1 0)) 19
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 1 1)) 21
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1 1 1)) 22
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1)) 20
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1)) 1
	(if (equal board '(1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1)) 5
	(if (equal board '(0 1 1 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 1 0 0 1)) 6
	(if (equal board '(0 0 1 0 0 0 1 1 0 0 1 1 0 0 0 1 0 0 0 0 0 1 0 0 1)) 7
	(if (equal board '(0 0 0 0 0 0 0 0 1 0 1 1 1 0 0 1 0 0 0 0 0 1 0 0 1)) 13
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 0 0 1 0 0 1 0 0 1)) 15
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 1 0 1 0 1 1 0 0 1)) 16
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 1 0 1 0 0 0 1)) 18
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 1 0 0 1 1)) 19
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1 0 0 1 0)) 20
	(if (equal board '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1 0)) 23

	nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
	)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
	))))))))))))))))))
)


