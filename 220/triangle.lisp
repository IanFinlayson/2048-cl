; draw a given row of the triangle
(defun draw-row (row size ch)
    ; leading spaces
    (loop for i from 1 to (- size row)
     do (format t " "))
    ; the fills
    (loop for i from 1 to row
     do (format t "~a " ch))
    ; new line
    (format t "~%"))

; draw a triangle of a given size, with a given character
(defun draw-triangle (size ch)
    (loop for i from 1 to size
     do (draw-row i size ch)))

; do it!
(format t "Enter size and fill: ")
(format t "~%")
(draw-triangle (read) (read))



