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

; make a board-widget class that derives from QWidget
(defclass board-widget ()
  ((board :accessor board)
   (board-labels :accessor board-labels))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("keyPressEvent" key-press-event)))

(defmethod left ((this board-widget))
  (format t "left~%"))
(defmethod right ((this board-widget))
  (format t "right~%"))
(defmethod up ((this board-widget))
  (format t "up~%"))
(defmethod down ((this board-widget))
  (format t "down~%"))

(defmethod key-press-event ((this board-widget) event)
  (cond ((= (#_key event) 16777234) (left this))
        ((= (#_key event) 16777235) (up this))
        ((= (#_key event) 16777236) (right this))
        ((= (#_key event) 16777237) (down this))))
        
; the constructor for the board-widget
(defmethod initialize-instance :after ((this board-widget) &key)
  (new this)
  ; clear the board
  (setf (board this)
    (loop for row from 0 to 3 collect
      (loop for col from 0 to 3 collect 0)))
  (setf (board-labels this)
    (loop for row from 0 to 3 collect
      (loop for col from 0 to 3 collect (#_new QLabel "Arf" this))))
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
    (#_setGeometry window 100 100 500 355)))

; call the main function
(main)

