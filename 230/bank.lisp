; read data from a file supplied by the user
(defun read-data ()
  (format t "Enter input file:~%")
  (with-open-file (f (read-line) :direction :input :if-does-not-exist nil)
    (if (null f)
      (format t "Error, file not found!~%")
      (loop for line = (read-line f nil nil) while line collect `(,line)))))


(format t "~a~%" (read-data))

