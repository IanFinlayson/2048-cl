; read the data in from an input file as a list of strings
(defun read-data ()
  (let ((data '()))
    (format t "Enter input file:~%")
    (with-open-file (f (read-line) :direction :input :if-does-not-exist nil)
      (if (null f)
        (progn 
         (format t "Error, file not found!~%")
         '())
        (progn
          (loop for line = (read-line f nil nil)
            while line
            do (setf data (cons line data)))
          (reverse data))))))

; print a single bar to the screen
(defun print-bar (line s)
  (let ((name (subseq line 0 (search " " line)))
        (c (parse-integer (subseq line (+ (search " " line) 1)))))
   (format t "~a " name)
   (loop for i from 1 to (- s (length name))
     do (format t " "))
   (loop for i from 1 to c
     do (format t "+"))
   (format t "~%")))

; print the complete chart to the screen
(defun print-chart (data s)
  (when (not (null data))
    (print-bar (car data) s)
    (print-chart (cdr data) s)))

; find the amount of leading spaces needed
(defun spaces (data)
  (let ((biggest 0))
    (loop for i from 0 to (- (length data) 1)
      do
        (when (> (search " " (nth i data)) biggest)
          (setf biggest (search " " (nth i data)))))
    biggest))

; main code
(let ((data (read-data)))
  (when (not (null data))
    (format t "~a~%" (car data))
    (loop for i from 1 to (length (car data))
      do (format t "-"))
    (format t "~%")
    (print-chart (cdr data) (spaces (cdr data)))))

