; break a string into pieces
(defun parse (str)
  (let ((result '()) (buff ""))
    (loop for i from 0 to (- (length str) 1) do
      (if (char= (char str i) #\Space)
        (progn (setf result (cons buff result))
          (setf buff ""))
        (setf buff (concatenate 'string buff (string (char str i))))))
    (reverse (cons buff result))))

; load a text file in and return a list of courses
(defun read-data (fname)
    (with-open-file (f fname :direction :input :if-does-not-exist nil)
      (if (null f)
        (format t "Error, file not found!~%")
        (loop for line = (read-line f nil nil) while line
          collect (parse line)))))

; make a graph out of the list of courses



; run it
(defun main (fname)
  (format t "~a~%" (read-data fname)))

  
  

; get the argument
;(if (< (length *posix-argv*) 2)
  ;(format t "Error, run with an argument!~%")
  ;(main (second *posix-argv*)))
(main "cs.txt")

