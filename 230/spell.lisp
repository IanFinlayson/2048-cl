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

; load a text file in and add each line to a binary search tree, returning it
(defun read-data ()
  (let ((tree nil))
    (with-open-file (f "words.txt" :direction :input :if-does-not-exist nil)
      (if (null f)
        (format t "Error, file not found!~%")
        (loop for line = (read-line f nil nil) while line do
          (setf tree (insert tree line)))))
    tree))

(format t "Height of tree is ~a.~%" (height (read-data)))


