; break a string into pieces
(defun parse (str)
  (let ((result '()) (buff ""))
    (loop for i from 0 to (- (length str) 1) do
      (if (char= (char str i) #\Space)
        (progn (setf result (cons buff result))
          (setf buff ""))
        (setf buff (concatenate 'string buff (string (char str i))))))
    (reverse (cons buff result))))

; insert a string into a binary search tree and return it
(defun insert (tree str)
  (if (null tree)
    `(,str ,nil ,nil)
    (if (string< str (car tree))
      `(,(car tree) ,(insert (cadr tree) str) ,(caddr tree))
      `(,(car tree) ,(cadr tree) ,(insert (caddr tree) str)))))

; find the height of a tree
(defun height (tree)
  (if (null tree)
    0
    (+ 1 (max (height (cadr tree)) (height (caddr tree))))))

; search the bst returning the string or nil
(defun look (tree str)
  (cond ((null tree) nil)
        ((string= str (car tree)) str)
        ((string< str (car tree)) (look (cadr tree) str))
        (t (look (caddr tree) str))))

; load a text file in and add each line to a binary search tree, returning it
(defun read-data ()
  (let ((tree nil))
    (with-open-file (f "words.txt" :direction :input :if-does-not-exist nil)
      (if (null f)
        (format t "Error, file not found!~%")
        (loop for line = (read-line f nil nil) while line do
          (setf tree (insert tree line)))))
    tree))

; main program loop
(defun main (tree)
  (let ((words (parse (read-line))))
    (unless (string= (car words) "END")
      (dolist (bad (remove-if (lambda (w) (look tree w)) words))
        (format t "~a is spelled wrong!~%" bad))
      (main tree))))

(let ((tree (read-data)))
  (format t "Loaded the words into a tree with height =  ~a.~%" (height tree))
  (main tree))

