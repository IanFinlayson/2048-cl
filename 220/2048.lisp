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
(defconstant +center-text+ 132)
(defconstant +font-size+ 28)
(defconstant +window-size+ 450)
(defconstant +cell-size+ 100)
(defconstant +left-key+ 16777234)
(defconstant +up-key+ 16777235)
(defconstant +right-key+ 16777236)
(defconstant +down-key+ 16777237)

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
    (#_drawText painter (#_new QRect 0 0 +cell-size+ +cell-size+) +center-text+ (write-to-string (value this)))
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

; method to update the labels
(defmethod update-board ((this board-widget))
  (loop for row from 0 to 3 do
    (loop for col from 0 to 3 do
      (#_update (index (board this) row col)))))

; these methods each move the board in a specific direction
; they return whether or not they made any change at all
(defmethod left ((this board-widget))
  (let ((change nil))
    ; for each row
    (loop for row from 0 to 3 do
      ; for each column in reverse
      (loop for col from 1 to 3 do
        ; find v (current) and l (left)
        (let ((v (value (index (board this) row col)))
              (l (value (index (board this) row (- col 1)))))
          ; when one to the left is empty, move it
          (when (and (= l 0) (/= v 0))
            (setf (value (index (board this) row (- col 1))) v)
            (setf (value (index (board this) row col)) 0)
            (setf change t))
          ; when one to the left is equal, combine it
          (when (and (= l v) (/= v 0))
            (setf (value (index (board this) row (- col 1))) (* v 2))
            (setf (value (index (board this) row col)) 0)
            (setf change t)))))
    change))


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

; this is the main key press handler for the game
(defmethod key-press-event ((this board-widget) event)
  (when (cond ((= (#_key event) +left-key+) (left this))
        ((= (#_key event) +up-key+) (up this))
        ((= (#_key event) +right-key+) (right this))
        ((= (#_key event) +down-key+) (down this))
        (t nil))
    (add-random this))
  (update-board this))
        
; the constructor for the board-widget
(defmethod initialize-instance :after ((this board-widget) &key)
  (new this)
  ; clear the board
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
  ; add a grid layout of labels
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

