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

; indexes a 2d list
(defun index (l row col)
  (nth col (nth row l)))

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
    (format t "Paint!~%")
      (#_setPen painter (#_new QColor 242 177 121))
      (#_setBrush painter (#_new QBrush (#_new QColor 242 177 121)))
      (#_drawRect painter (#_new QRect 0 0 100 100))
      (#_setPen painter (#_new QColor 255 255 255))
      (#_setFont painter (#_new QFont "Arial" 28))
      (#_drawText painter (#_new QRect 0 0 100 100) 132 (write-to-string (value this)))
      (#_end painter)))

; constructor for cell
(defmethod initialize-instance :after ((this cell-widget) &key val)
  (setf (value this) val)
  (new this))

; make a board-widget class that derives from QWidget
(defclass board-widget ()
  ((board :accessor board)
   (board-labels :accessor board-labels))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("keyPressEvent" key-press-event)))

; method to update the labels
(defmethod update-board ((this board-widget))
  (loop for row from 0 to 3 do
    (loop for col from 0 to 3 do
      (format t "TODO~%"))))
          ;      (#_setText (index (board-labels this) row col) (write-to-string (index (board this) row col))))))

; these methods each move the board in a specific direction
; they return whether or not they made any change at all
(defmethod left ((this board-widget))
  (format t "left~%")
  t)
(defmethod right ((this board-widget))
  (format t "right~%")
  t)
(defmethod up ((this board-widget))
  (format t "up~%")
  t)
(defmethod down ((this board-widget))
  (format t "down~%")
  t)

; this method checks if the board is full
(defmethod fullp ((this board-widget))
  (let ((full t))
    (loop for row from 0 to 3 do
      (loop for col from 0 to 3 do
        (when (= (index (board this) row col) 0)
          (setf full nil))))
    full))

; this method adds a random 2 or 4 cell into an empty space
(defmethod add-random ((this board-widget))
  (when (not (fullp this))
    (let ((row (random 4)) (col (random 4)))
      (if (= (index (board this) row col) 0)
        (setf (nth col (nth row (board this))) (if (= (random 2) 0) 2 4))
        (add-random this)))))

; this is the main key press handler for the game
(defmethod key-press-event ((this board-widget) event)
  (when (cond ((= (#_key event) 16777234) (left this))
        ((= (#_key event) 16777235) (up this))
        ((= (#_key event) 16777236) (right this))
        ((= (#_key event) 16777237) (down this))
        (t nil))
    (add-random this))
  (update-board this))
        
; the constructor for the board-widget
(defmethod initialize-instance :after ((this board-widget) &key)
  (new this)
  ; clear the board
  (setf (board this)
    (loop for row from 0 to 3 collect
      (loop for col from 0 to 3 collect 0)))
  (setf (board-labels this)
    (loop for row from 0 to 3 collect
      ;(loop for col from 0 to 3 collect (#_new QLabel "0" this))))
      (loop for col from 0 to 3 collect (make-instance 'cell-widget :val 0))))
  ; add two randoms and update the board
  (add-random this)
  (add-random this)
  (update-board this)
  ; initialize the space (x, y, w, h) and title
  (#_setGeometry this 200 200 500 500)
  (#_setWindowTitle this "2048 Game")
  ; add a grid layout of labels
  (let ((grid (#_new QGridLayout this)))
    (loop for row from 0 to 3 do
      (loop for col from 0 to 3 do
        (#_addWidget grid (index (board-labels this) row col) row col)))
    (#_setLayout this grid)))

; make a new window
(defun main ()
  (with-main-window (window (make-instance 'board-widget))
    (#_setStyleSheet window "background-color: #C2B39A;")
    (#_setFixedSize window 450 450)))

; call the main function
(setf *random-state* (make-random-state t))
(main)

