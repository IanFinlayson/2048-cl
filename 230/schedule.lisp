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

; make a list of just the nodes
(defun make-nodes (data)
  (loop for course in data
    collect (first course)))

; make an adjacency matrix
(defun make-matrix (data)
  (loop for a in data collect
    (loop for b in data collect
      (if (member (first b) (rest a) :test #'string=) t nil))))

; returns a list of courses with no edges coming in
(defun find-active (matrix)
  (loop for i from 0 to (length matrix) collect
    (if (member t (nth i matrix)) i)))

; do the topological sorting
(defun top-sort (nodes matrix)
  (let ((ordering '())
        (active (find-active matrix)))
    ))
    

; run it
(defun main (fname)
  (let* ((data (read-data fname))
         (nodes (make-nodes data))
         (matrix (make-nodes data)))
    (top-sort nodes matrix)))

; get the argument
;(if (< (length *posix-argv*) 2)
  ;(format t "Error, run with an argument!~%")
  ;(main (second *posix-argv*)))
(main "cs.txt")


