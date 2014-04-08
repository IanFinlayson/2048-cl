; a die roll
(defun dice ()
  (let ((r (+ (random 6) 1)))
    (format t "Rolled a ~a~%" r)
    r))

; the human players turn
(defun human (hscore cscore turn)
  (format t "Your score: ~a, Computer score: ~a~%" hscore cscore)
  (format t "So far this round: ~a~%" turn)
  (format t "Would you like to roll?~%")
  (if (>= (+ hscore turn) 100)
    (+ hscore turn)
    (if (y-or-n-p)
      (let ((r (dice)))
        (if (= r 1)
          hscore
          (human hscore cscore (+ turn r))))
      (+ hscore turn))))
(defun human-turn (hscore cscore)
  (format t "It is your turn!~%")
  (let ((s (human hscore cscore 0)))
    (format t "Your new score is ~a~%" s)
    s))

; the computer's turn
(defun computer (cscore turn)
  (cond ((>= turn 20) (+ cscore turn))
        ((>= (+ cscore turn) 100) (+ cscore turn))
        (t (let ((r (dice)))
          (if (= r 1)
            cscore
            (computer cscore (+ turn r)))))))
(defun computer-turn (cscore)
  (let ((s (computer cscore 0)))
    (format t "After its turn, the computer's score is ~a~%" s)
    s))

; run one game
(defun game (hscore cscore player-turn)
  (cond ((>= hscore 100) 'Human)
        ((>= cscore 100) 'Computer)
        (player-turn (game (human-turn hscore cscore) cscore (not player-turn)))
        (t (game hscore (computer-turn cscore) (not player-turn)))))
    
; main program
(defun main ()
  (format t
    (if (eq (game 0 0 (= (random 2) 0)) 'Human)
      "Congratulations, you won!!!~%"
      "Sorry, the computer won.~%"))
  (format t "Would you like to play again?~%")
  (if (y-or-n-p)
   (main)
   nil))

; seed the rng and run it
(setf *random-state* (make-random-state t))
(main)

