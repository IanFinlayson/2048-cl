; break a string into pieces
(defun parse (str)
  (let ((result '()) (buff ""))
    (loop for i from 0 to (- (length str) 1)
      do
        (if (char= (char str i) #\Space)
          (progn (setf result (cons buff result))
            (setf buff ""))
          (setf buff (concatenate 'string buff (string (char str i))))))
    (setf result (cons buff result))
    (reverse result)))

(defun tonums (l)
  `(,(car l) ,(parse-integer (cadr l)) ,(parse-integer (caddr l))))

; read data from a file supplied by the user
(defun read-data ()
  (format t "Enter input file:~%")
  (with-open-file (f (read-line) :direction :input :if-does-not-exist nil)
    (if (null f)
      (format t "Error, file not found!~%")
      (loop for line = (read-line f nil nil) while line collect (tonums (parse line))))))

; increment the time of day correctly
(defun inc-time (now)
  (if (= (mod now 100) 59)
    (* (+ (floor (/ now 100)) 1) 100)
    (+ 1 now)))

; queue functions
(defun enqueue (q item)
  (append q (list `(,(car item) ,(+ (cadr item) (caddr item))))))
(defun front (q)
  (car q))
(defun dequeue (q)
  (cdr q))

; main program simulation
(defun sim (minute queue data teller)
  ;(format t "Time is ~a, queue is ~a~%" minute queue)
  (cond ((and (not (null data)) (= minute (cadar data)))
        (progn
          (format t "~a got in line at ~a.~%" (caar data) minute)
          (sim minute (enqueue queue (car data)) (cdr data) teller)))
        ((and (not (null teller)) (= minute (cadr teller)))
         (progn
           (format t "~a is done at ~a.~%" (caar queue) minute)
           (sim minute (dequeue queue) data (front queue))))
        ((>= minute 1700) nil)
        (t (sim (inc-time minute) queue data teller))))

; start it all in motion
(sim 900 '() (read-data) nil)

