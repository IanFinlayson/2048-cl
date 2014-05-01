;; 2048 game written in Common Lisp with QT
;; Ian Finlayson

; load up quicklisp and CommonQT
(load "~/quicklisp/setup.lisp")
(ql:quickload 'qt)

; create a package and export the main function
(defpackage :twenty-forty-eight
  (:use :cl :qt)
  (:export #:main))
(in-package :twenty-forty-eight)

; allow us to read qt symbols directly
(named-readtables:in-readtable :qt)

; constants
(defconstant +font-size+ 24)
(defconstant +window-size+ 450)
(defconstant +cell-size+ 100)

; indexes a 2d list
(defun index (l row col)
  (nth col (nth row l)))

; this function returns the background color for a given value
(defun bg-color (val)
  (case val
    (0 (#_new QColor 204 192 179))
    (2 (#_new QColor 238 228 218))
    (4 (#_new QColor 237 224 200))
    (8 (#_new QColor 242 177 121))
    (16 (#_new QColor 245 149 99))
    (32 (#_new QColor 246 124 95))
    (64 (#_new QColor 246 94 59))
    (128 (#_new QColor 237 207 114))
    (256 (#_new QColor 237 204 97))
    (512 (#_new QColor 237 200 80))
    (1024 (#_new QColor 237 197 63))
    (2048 (#_new QColor 237 194 46))
    (otherwise (#_new QColor 47 43 37))))

; this function returns the text color for a given value
(defun text-color (val)
  (case val
    (0 (#_new QColor 204 192 179))
    ((2 4) (#_new QColor 119 110 101))
    (otherwise (#_new QColor 249 246 242))))

; this function asks a yes/no question with a dialog and returns the response
(defun ask-question (title text)
  (let ((dialog (#_new QMessageBox)))
    (#_setWindowTitle dialog title)
    (#_setText dialog text)
    (#_addButton dialog (#_QMessageBox::No))
    (#_addButton dialog (#_QMessageBox::Yes))
    (= (#_exec dialog) (enum-value (#_QMessageBox::Yes)))))

; this class represents a single cell in the game
(defclass cell-widget ()
  ((value :accessor value))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("paintEvent" paint-event)))

; the paint method for the cell, it draws the cell to the screen
(defmethod paint-event ((this cell-widget) paint-event)
  (declare (ignore paint-event))
  (let ((painter (#_new QPainter this)))
    (#_setPen painter (bg-color (value this)))
    (#_setBrush painter (#_new QBrush (bg-color (value this))))
    (#_drawRect painter (#_new QRect 0 0 +cell-size+ +cell-size+))
    (#_setPen painter (text-color (value this)))
    (#_setFont painter (#_new QFont "Arial" +font-size+))
    (#_drawText painter (#_new QRect 0 0 +cell-size+ +cell-size+) (enum-value (#_Qt::AlignCenter)) (write-to-string (value this)))
    (#_end painter)))

; constructor for cell
(defmethod initialize-instance :after ((this cell-widget) &key val)
  (setf (value this) val)
  (new this))

; make a board-widget class that derives from QWidget
(defclass board-widget ()
  ((board :accessor board))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("keyPressEvent" key-press-event)))

; method to update the widget
(defmethod update-board ((this board-widget))
  (loop for row from 0 to 3 do
    (loop for col from 0 to 3 do
      (#_update (index (board this) row col)))))

; these methods each move the board in a specific direction
; they return whether or not they made any change at all
(defmethod left ((this board-widget))
  ; assume no change, set to true on change
  (let ((change nil))
    ; for each row
    (loop for i from 0 to 3 do
      ; for each column
      (loop for j from 0 to 3 do
        ; when this cell is not zero
        (when (/= (value (index (board this) i j)) 0)
          ; for each cell from here to 1
          (loop for k from j downto 1 do
            ; if it can move to the right
            (when (= (value (index (board this) i (- j k))) 0)
              ; move it
              (setf (value (index (board this) i (- j k))) (value (index (board this) i j)))
              (setf (value (index (board this) i j)) 0)
              (setf change t)))))
      ; for each column
      (loop for j from 0 to 3 do
        ; if it can be combined
        (when (and (< (+ j 1) 4) (= (value (index (board this) i j)) (value (index (board this) i (+ j 1)))))
          ; combine it
          (setf (value (index (board this) i j)) (* (value (index (board this) i j)) 2))
          (setf (value (index (board this) i (+ j 1))) 0)
          (when (/= (value (index (board this) i j)) 0)
            (setf change t))))
      ; check again to see if we can move things to the left
      (loop for j from 0 to 3 do
        (when (/= (value (index (board this) i j)) 0)
          (loop for k from j downto 1 do
            (when (= (value (index (board this) i (- j k))) 0)
              (setf (value (index (board this) i (- j k))) (value (index (board this) i j)))
              (setf (value (index (board this) i j)) 0)
              (setf change t))))))
    ; whether there was any change
    change))

; this function rotates the board by 90 degrees counter-clockwise
(defmethod rotate ((this board-widget))
  (setf (board this) (reverse (apply #'mapcar #'list (board this)))))

; to move right, rotate twice, left, rotate twice
(defmethod right ((this board-widget))
  (let ((change nil))
    (dotimes (i 2) (rotate this))
    (setf change (left this))
    (dotimes (i 2) (rotate this))
    change))
    
; to move up, rotate once, left, rotate 3 times
(defmethod up ((this board-widget))
  (let ((change nil))
    (rotate this)
    (setf change (left this))
    (dotimes (i 3) (rotate this))
    change))

; to move down rotate three times, left, rotate once
(defmethod down ((this board-widget))
  (let ((change nil))
    (dotimes (i 3) (rotate this))
    (setf change (left this))
    (rotate this)
    change))

; this method checks if the board is full
(defmethod fullp ((this board-widget))
  (let ((full t))
    (loop for row from 0 to 3 do
      (loop for col from 0 to 3 do
        (when (= (value (index (board this) row col)) 0)
          (setf full nil))))
    full))

; this method adds a random 2 or 4 cell into an empty space
(defmethod add-random ((this board-widget))
  (when (not (fullp this))
    (let ((row (random 4)) (col (random 4)))
      (if (= (value (index (board this) row col)) 0)
        (setf (value (nth col (nth row (board this)))) (if (= (random 2) 0) 2 4))
        (add-random this)))))

; this method returns whether the game is lost TODO this fails because of 3
(defmethod lostp ((this board-widget))
  ; assume that we did lose
  (let ((lost t))
    ; check the verticals
    (loop for i from 0 to 3 do
      (loop for j from 0 to 2 do
        (when (= (value (index (board this) i j)) (value (index (board this) i (+ j 1))))
          (setf lost nil))))
    ; check the horizontals
    (loop for i from 0 to 2 do
      (loop for j from 0 to 3 do
        (when (= (value (index (board this) i j)) (value (index (board this) (+ i 1) j)))
          (setf lost nil))))
    ; check for zeroes
    (when (not (fullp this))
      (setf lost nil))
    lost))
        
; this method returns whether the game is won
(defmethod wonp ((this board-widget))
  (let ((won nil))
    (loop for i from 0 to 3 do
      (loop for j from 0 to 3 do
        (when (= (value (index (board this) i j)) 2048)
          (setf won t))))
    won))

; this method checks for loss and win, and updates accordingly
(defmethod check-end ((this board-widget))
  (let ((quit nil) (reset nil))
    ; if the game is won or lost, ask to reset
    (when (lostp this)
      (if (ask-question "Sorry" "You lost! Play again?")
        (setf reset t)
        (setf quit t)))
    (when (wonp this)
      (if (ask-question "Congratulations" "You won! Play again?")
        (setf reset t)
        (setf quit t)))
    (when reset
      (loop for row from 0 to 3 do
        (loop for col from 0 to 3 do
          (setf (value (index (board this) row col)) 0)))
      (add-random this)
      (add-random this))
    (when quit
      (#_QCoreApplication::exit 0))))
  
; this is the main key press handler for the game
(defmethod key-press-event ((this board-widget) event)
  (when (cond ((= (#_key event) (enum-value (#_Qt::Key_Left))) (left this))
        ((= (#_key event) (enum-value (#_Qt::Key_Up))) (up this))
        ((= (#_key event) (enum-value (#_Qt::Key_Right))) (right this))
        ((= (#_key event) (enum-value (#_Qt::Key_Down))) (down this))
        (t nil))
    (add-random this)
    (update-board this)
    (check-end this)))
        
; the constructor for the board-widget
(defmethod initialize-instance :after ((this board-widget) &key)
  (new this)
  (setf (board this)
    (loop for row from 0 to 3 collect
      (loop for col from 0 to 3 collect
        (make-instance 'cell-widget :val 0))))
  ; add two randoms and update the board
  (add-random this)
  (add-random this)
  (update-board this)
  ; initialize the space (x, y, w, h) and title
  (#_setWindowTitle this "2048 Game")
  ; add a grid layout of cells
  (let ((grid (#_new QGridLayout this)))
    (loop for row from 0 to 3 do
      (loop for col from 0 to 3 do
        (#_addWidget grid (index (board this) row col) row col)))
    (#_setLayout this grid)))

; make a new window
(defun main ()
  (with-main-window (window (make-instance 'board-widget))
    (#_setStyleSheet window "background-color: #C2B39A;")
    (#_setFixedSize window +window-size+ +window-size+)))

; call the main function
(setf *random-state* (make-random-state t))
(main)

