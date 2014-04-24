; break a string into pieces
(defun parse (str)
  (let ((result '()) (buff ""))
    (loop for i from 0 to (- (length str) 1) do
      (if (char= (char str i) #\Space)
        (progn (setf result (cons buff result))
          (setf buff ""))
        (setf buff (concatenate 'string buff (string (char str i))))))
    (reverse (cons buff result))))

; convert strings in a list to numbers
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

; subtract two time of days correctly
(defun time-sub (a b)
  (-  (+ (* (floor (/ a 100)) 60) (mod a 100))
      (+ (* (floor (/ b 100)) 60) (mod b 100))))

; main simulation function
(defun sim ()
  (let ((minute 900) (queue '()) (data (read-data)) (next nil) (total 0) (peeps 0))
    (loop while (or next queue data) do
      ;(format t "next ~a queue ~a~%~%" next queue)
      ; if the person at the teller is done
      (when (and next (= (caddr next) 0))
        (incf peeps)
        (incf total (time-sub minute (cadr next)))
        (format t "~a is done at ~a.~%" (car next) minute)
        ; move to next person in queue
        (if (null queue)
          (setf next nil)
          (progn (setf next (car queue)) (setf queue (cdr queue)))))
      ; if the next person is here
      (when (and data (= (cadar data) minute))
        (format t "~a got in line at ~a.~%" (caar data) minute)
        ; move them to the queue
        (setf queue (append queue `(,(car data))))
        (setf data (cdr data)))
      ; if there's a queue, and nobody is next
      (when (and (null next) queue)
        (setf next (car queue))
        (setf queue (cdr queue)))
      ; decrement the person waiting
      (when (not (null next))
        (decf (caddr next)))
      ; tick tock
      (setf minute (inc-time minute)))
    (/ total peeps)))

; call it!
(format t "Average wait time is ~4$ minutes.~%" (sim))

