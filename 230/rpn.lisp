; an error condition
(define-condition bad-input (error) ())

; function to parse a string into a list of strings by spaces
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

; tests if a string is a number
(defun is-number (str)
  (let ((*read-eval* nil))
   (ignore-errors (numberp (read-from-string str)))))

; do a stack operation, giving another stack
(defun do-op (stack op)
  (if (< (list-length stack) 2)
     (error 'bad-input :text "")
     (cons (funcall op (cadr stack) (car stack)) (cdr (cdr stack)))))

; evaluate function
(defun evaluate (input stack)
  (if (null input)
    (progn
      (if (= (list-length stack) 1)
        (car stack)
        (error 'bad-input :text "")))
    (cond ((string= (car input) "+") (evaluate (cdr input) (do-op stack #'+)))
          ((string= (car input) "-") (evaluate (cdr input) (do-op stack #'-)))
          ((string= (car input) "*") (evaluate (cdr input) (do-op stack #'*)))
          ((string= (car input) "/") (evaluate (cdr input) (do-op stack #'/)))
          ((string= (car input) "^") (evaluate (cdr input) (do-op stack #'expt)))
          ((is-number (car input)) (evaluate (cdr input) (cons (with-input-from-string (in (car input)) (read in)) stack)))
          (t (error 'bad-input :text "")))))

; main program loop
(defun main ()
  (format t "> ")
  (finish-output nil)
  (let ((line (read-line)))
    (when (> (length line) 0)
      (handler-case (progn
        (format t "~a~%" (evaluate (parse line) '()))
        (main))
       (bad-input (b) (progn
         (format t "Invalid Expression!~%")
         (main)))))))

; get going
(main)

