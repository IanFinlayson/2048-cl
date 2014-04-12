; get the message and key from user, and return as list
(defun input (op pre)
  (format t "Enter message:~%")
  (let ((m (read-line)))
    (format t "Enter key~%")
      (do-encdec m (read-line) op pre)))

; rotate the key around (abc -> bca)
(defun rotate (k)
  (concatenate 'string (subseq k 1) (subseq k 0 1)))

; wrap a value into the range we need
(defun wrap (m)
  (cond ((< m 33) (wrap (+ m 93)))
        ((> m 126) (wrap (- m 93)))
        (t m)))

; encdec a single char
(defun encdec-char (m k op)
  (if (or (< (char-code m) 33) (> (char-code m) 126))
    m
    (code-char (wrap (funcall op (char-code m) (char-code k))))))

; encdec a string and return it
(defun encdec (m k accum op)
  (if (= (length m) 0)
    accum
    (encdec (subseq m 1) (rotate k) (concatenate 'string accum (list (encdec-char (char m 0) (char k 0) op))) op)))

; do the encode/decode operation
(defun do-encdec (m k op prefix)
  (format t "~acoded string is ~a~%" prefix (encdec m k "" op)))

; run the program
(format t "Would you like to encode?~%")
(if (y-or-n-p)
  (input #'+ "En")
  (input #'- "De"))

