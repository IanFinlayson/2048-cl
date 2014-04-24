; print a single digit correctly
(defun print-digit (d)
  (format t "~a"
    (if (< d 10)
      d
      (code-char (+ (- d 10) (char-code #\A))))))

; print a number in a given base
(defun print-base (n b)
  (when (/= n 0)
    (print-base (floor (/ n b)) b)
    (print-digit (mod n b))))

; keep recursing until number is 0
(defun main ()
  (format t "~%Enter a number and base: ")
  (let ((n (read)) (b (read)))
    (when (and (> n 0) (> b 0))
      (print-base n b)
      (main))))

; start it off
(main)

