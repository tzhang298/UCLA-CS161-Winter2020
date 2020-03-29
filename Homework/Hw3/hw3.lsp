;
; CS161 Hw3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy
; Allegro).
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
;
; In either case, this limitation should not significantly affect your grade.
;
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
;
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

  (defun isKeeper (v)
    (= v keeper)
    )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     )
	   )
	)
)

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
;
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond (
    (null s) nil)
    (t
      (let ((x (getKeeperColumn (car s) 0)))
        (if x
		      (list x row)
		      (getKeeperPosition (cdr s) (+ row 1))
		    )
      )
    )
	)
)

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond (
    (null L) nil)
    (t
      (let ((cur (car L)) (res (cleanUpList (cdr L))))
        (if cur
		      (cons cur res)
		      res
		    )
      )
    )
	)
)

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
; goal-test : Recursion to make sure no boxes left return nil if there is still box
(defun goal-test (s)
  (cond
    ((null s) T)
    ((atom s)
      (cond
        ((isBox s) nil)
        (t T)
      )
    )
    (t (and (goal-test (car s)) (goal-test (cdr s))))
  )
)

; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
;
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;
;

; get-col is a helper function for get-square that returns the integer
; contents of this row at column c
(defun get-col (row c)
  (let ((curr-elem (car row)) (other-elems (cdr row)))
    (cond
      ((null row) wall)
      ((< c 0) wall)
      ((= c 0) curr-elem)
      (t (get-col other-elems (- c 1)))
    ))
)

; get-row is a helper function for get-square and returns the row
; we want
(defun get-row (s r)
  (let ((curr-row (car s)) (other-rows (cdr s)))
    (cond
      ((null s) nil)
      ((< r 0) nil)
      ; We found the row we want
      ((= r 0) curr-row)
      ; Recursion to find the row we want
      (t (get-row other-rows (- r 1)))
      ))
)

; get-square is a helper function of try-move, it returns the integer content of state S at square (r, c).
; If the square is outside the
; scope of the problem, it returns the value of the wall
(defun get-square (s r c)
  (let ((row (get-row s r)))
    (get-col row c)))

; set-row is a helper function for set-square and sets square with col c of this
; row with value v
(defun set-row (row c v)
  (let ((curr-elem (car row)) (other-elems (cdr row)))
    (cond
      ((null row) nil)
      ((= c 0) (cons v other-elems))
      ; Recursively iterate through elems of this row until we find the column we want
      (t (cons curr-elem (set-row other-elems (- c 1) v)))
    ))
)

; set-square takes four arguments returns a new state S'
; that is created by setting square (r, c) to v. And
; does not modify the input state
(defun set-square (s r c v)
  (let ((curr-row (car s)) (other-rows (cdr s)))
    (cond
      ((null s) nil)
      ((= r 0) (cons (set-row curr-row c v) other-rows))
      (t (cons curr-row (set-square other-rows (- r 1) c v)))
    ))
)

; can-move is a helper function for try-move that takes two arguments and returns true if the keeper can
; move in move direction
(defun can-move (one-away two-away)
  (cond
    ((isWall one-away) nil)
    ((and (isBox one-away) (not (or (isStar two-away) (isBlank two-away)))) nil)
    (t T)
  )
)

; do-move is a helper function for try-move that takes eight arguments and updates
; the corresponding contents (at most 3 squares) of the current state by moving in move direction
(defun do-move (s dir k r c op one-away two-away)
  (cond
    ; Moving up or down
    ((equal dir 'VERTICAL)
      (cond
        ;there are six different conditions and two sub conditions 1. is keeper 2. is keeperstar
        ((and (isKeeper k) (isBlank one-away))
          (set-square (set-square s r c blank) (funcall op r 1) c keeper))
        ((and (isKeeperStar k) (isBlank one-away))
          (set-square (set-square s r c star) (funcall op r 1) c keeper))

        ((and (isKeeper k) (isStar one-away))
          (set-square (set-square s r c blank) (funcall op r 1) c keeperstar))
        ((and (isKeeperStar k) (isStar one-away))
          (set-square (set-square s r c star) (funcall op r 1) c keeperstar))

        ((and (isKeeper k) (isBox one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c blank) (funcall op r 1) c keeper) (funcall op r 2) c box))
        ((and (isKeeperStar k) (isBox one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c star) (funcall op r 1) c keeper) (funcall op r 2) c box))

        ((and (isKeeper k) (isBox one-away) (isStar two-away))
          (set-square (set-square (set-square s r c blank) (funcall op r 1) c keeper) (funcall op r 2) c boxstar))
        ((and (isKeeperStar k) (isBox one-away) (isStar two-away))
          (set-square (set-square (set-square s r c star) (funcall op r 1) c keeper) (funcall op r 2) c boxstar))

        ((and (isKeeper k) (isBoxStar one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c blank) (funcall op r 1) c keeperstar) (funcall op r 2) c box))
        ((and (isKeeperStar k) (isBoxStar one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c star) (funcall op r 1) c keeperstar) (funcall op r 2) c box))

        ((and (isKeeper k) (isBoxStar one-away) (isStar two-away))
          (set-square (set-square (set-square s r c blank) (funcall op r 1) c keeperstar) (funcall op r 2) c boxstar))
        ((and (isKeeperStar k) (isBoxStar one-away) (isStar two-away))
          (set-square (set-square (set-square s r c star) (funcall op r 1) c keeperstar) (funcall op r 2) c boxstar))
      )
    )
    ; Moving left or right
    ((equal dir 'HORIZONTAL)
      (cond
      ;there are six different conditions and two sub conditions 1. is keeper 2. is keeperstar 
        ((and (isKeeper k) (isBlank one-away))
          (set-square (set-square s r c blank) r (funcall op c 1) keeper))
        ((and (isKeeperStar k) (isBlank one-away))
          (set-square (set-square s r c star) r (funcall op c 1) keeper))

        ((and (isKeeper k) (isStar one-away))
          (set-square (set-square s r c blank) r (funcall op c 1) keeperstar))
        ((and (isKeeperStar k) (isStar one-away))
          (set-square (set-square s r c star) r (funcall op c 1) keeperstar))

        ((and (isKeeper k) (isBox one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c blank) r (funcall op c 1) keeper) r (funcall op c 2) box))
        ((and (isKeeperStar k) (isBox one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c star) r (funcall op c 1) keeper) r (funcall op c 2) box))

        ((and (isKeeper k) (isBox one-away) (isStar two-away))
          (set-square (set-square (set-square s r c blank) r (funcall op c 1) keeper) r (funcall op c 2) boxstar))
        ((and (isKeeperStar k) (isBox one-away) (isStar two-away))
          (set-square (set-square (set-square s r c star) r (funcall op c 1) keeper) r (funcall op c 2) boxstar))

        ((and (isKeeper k) (isBoxStar one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c blank) r (funcall op c 1) keeperstar) r (funcall op c 2) box))
        ((and (isKeeperStar k) (isBoxStar one-away) (isBlank two-away))
          (set-square (set-square (set-square s r c star) r (funcall op c 1) keeperstar) r (funcall op c 2) box))

        ((and (isKeeper k) (isBoxStar one-away) (isStar two-away))
          (set-square (set-square (set-square s r c blank) r (funcall op c 1) keeperstar) r (funcall op c 2) boxstar))
        ((and (isKeeperStar k) (isBoxStar one-away) (isStar two-away))
          (set-square (set-square (set-square s r c star) r (funcall op c 1) keeperstar) r (funcall op c 2) boxstar))
      )
    )
  )
)

; try-move takes state S and move direction D, and returns the state that is the result
; of moving the keeper in state S in direction D. return NIL if the move is invalid
(defun try-move (s dir)
  ; Gets the integer contents of the keeper (i.e. keeper or keeperstar)
  (let* (
    (pos (getKeeperPosition s 0))
    (c (car pos))
    (r (cadr pos))
    (k (get-square s r c)))

    (cond
      ; Move up
      ((equal dir 'UP)
        ; Get the integer contents of the squares one and two steps away
        (let* ((one-away (get-square s (- r 1) c)) (two-away (get-square s (- r 2) c)))
          (cond
            ; If keeper can move in the move direction, then update the state
            ((equal (can-move one-away two-away) T)
              (do-move s 'VERTICAL k r c #'- one-away two-away)
            )
          )
        )
      )
      ; Move down similar to move up
      ((equal dir 'DOWN)
        (let* ((one-away (get-square s (+ r 1) c)) (two-away (get-square s (+ r 2) c)))
          (cond
            ((equal (can-move one-away two-away) T)
              (do-move s 'VERTICAL k r c #'+ one-away two-away)
            )
          )
        )
      )
      ; Move left
      ((equal dir 'LEFT)
        (let* ((one-away (get-square s r (- c 1))) (two-away (get-square s r (- c 2))))
          (cond
            ((equal (can-move one-away two-away) T)
              (do-move s 'HORIZONTAL k r c #'- one-away two-away)
            )
          )
        )
      )
      ; Move right
      ((equal dir 'RIGHT)
        (let* ((one-away (get-square s r (+ c 1))) (two-away (get-square s r (+ c 2))))
          (cond
            ((equal (can-move one-away two-away) T)
              (do-move s 'HORIZONTAL k r c #'+ one-away two-away)
            )
          )
        )
      )
    )
  )
)

(defun next-states (s)
  (cleanUpList (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT)))
)

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
; h0 is a trivially admissible heuristic function
; h0 takes a single argument: s (the current state) and just returns the constant 0
(defun h0 (s)
  0
);end defun

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
; h1 is an admissible heuristic function (hamming distance) that is admissible
; any boxes not on stars need to move at least once
; h1 takes a single argument: s (the current state) and returns the number of boxes not
; on goal squares

(defun h1 (s)
  (cond
    ((null s) 0)
    ((atom s)
      (cond
        ((isBox s) 1)
        (t 0)
      );end cond
    )
    (t (+ (h1 (car s)) (h1 (cdr s))))
  );end cond
); end defun

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;

;helper function for min-dis to calculate distances from box to star
(defun dis-between (box-pos star-pos)
  (let ((box-row (first box-pos)) (box-col (second box-pos)) (star-row (first star-pos)) (star-col (second star-pos)))
    (cond
      ((null box-pos) 0)
      ((null star-pos) 0)
      ((< box-row star-row)
        (cond
          ((< box-col star-col) (+ (- star-row box-row) (- star-col box-col)))
          (t (+ (- star-row box-row) (- box-col star-col)))
        )
      )
      (t
        (cond
          ((< box-col star-col) (+ (- box-row star-row) (- star-col box-col)))
          (t (+ (- box-row star-row) (- box-col star-col)))
        )
      )
    )
  )
)

;helper function for cal-dis returns the distance from the box
; to the closest star square
(defun min-dis (box-pos star-position)
  (let ((star-pos (car star-position)) (other-stars (cdr star-position)))
    (cond
      ((not box-pos) nil)
      ((not star-position) nil)
      ; Get the current distance from the box to a single star, and distances from the box to the remaining stars
      (t (let ((curr-dist (dis-between box-pos star-pos)) (next-dist (min-dis box-pos other-stars)))
        (cond
          ((not next-dist) curr-dist)
          (t
            (cond
              ; Return the min distance from the box to a star
              ((< curr-dist next-dist) curr-dist)
              (t next-dist)
            )
          )
        ))
      )
    )
  )
)

; cal-dis is also a helper function to h805332081 it takes box-position (a list of coordinates of all boxes)
; and star-position (a list of coordinates of all squares),
; and calculate the manhattan distance of boxes to get to star squares
(defun cal-dis (box-position star-position)
    (let ((box-pos (car box-position)) (other-boxs (cdr box-position)))
      (cond
        ((not box-pos) 0)
        ((not star-position) 0)
        ; Recursively calculates the min distance for each box to get to a star square, and adds them up
        (t (+ (min-dis box-pos star-position) (cal-dis other-boxs star-position)))
      )
    )
)



;helper function: returns a list of coordinates of
;either boxes or stars for the current row

(defun set-position (row-num col-nums)
  (let ((curr-col (car col-nums)) (other-cols (cdr col-nums)))
    (cond
      ((null col-nums) nil)
      (t (append (list (list row-num curr-col)) (set-position row-num other-cols)))
    )
  )
)

; helper function: returns the column numbers
; of all boxes or all stars in the current row
(defun getcols (row object-type col-num)
  (let ((curr-elem (car row)) (other-elems (cdr row)))
    (cond
      ((null row) nil)
      (t
        (cond
          ((equal object-type box)
            (cond
              ; add box and recursively look for other boxes in this row
              ((isBox curr-elem) (cons col-num (getcols other-elems object-type (+ col-num 1))))
              (t (getcols other-elems object-type (+ col-num 1)))
            )
          )
          ((equal object-type star)
            (cond
              ; add star and look for next
              ((isStar curr-elem) (cons col-num (getcols other-elems object-type (+ col-num 1))))
              (t (getcols other-elems object-type (+ col-num 1)))
            )
          )
        )
      )
    )
  )
)

;get-position is a helper function to h805332081, it takes three arguments: s (the current state), object-type
; (whether it is a box or star), and row-num, and returns the position of either all boxes or all stars

(defun get-position (s object-type row-num)
  (let ((curr-row (car s)) (other-rows (cdr s)))
    (cond
      ((null s) nil)
      (t (let* ((cols (getcols curr-row object-type 0)) (position (set-position row-num cols)))
        (cond
          ; Recursively find rows to get position of boxes or stars
          ((not (equal cols nil)) (append position (get-position other-rows object-type (+ row-num 1))))
          (t (get-position other-rows object-type (+ row-num 1)))
        ))
      )
    )
  )
)

(defun h805332081 (s)
  (let ((box-position (get-position s box 0)) (star-position (get-position s star 0)))
    ; Calculate the manhattan distance from each box to get to a star square
    (cal-dis box-position star-position)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1)
	   (1 0 0 0 0 0 1)
	   (1 0 0 2 1 4 1)
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1)
	   (1 1 1 0 0 1 1 1 1)
	   (1 0 0 0 0 0 2 0 1)
	   (1 0 1 0 0 1 2 0 1)
	   (1 0 4 0 4 1 3 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
