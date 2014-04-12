


; evaluate function
(defun evaluate (l i stack)
  (let ((val (multiple-value-list (read-from-string l))))
    (cond ((string= (car val) "+")) (evaluate
    

; main program loop
(defun main ()
  (format t "> ")
  (finish-output nil)
  (let ((line (read-line)))
    (when (> (length line) 0)
      (progn
        (format t "~a~%" (evaluate line 0 '()))
        (main)))))

; get going
(main)
