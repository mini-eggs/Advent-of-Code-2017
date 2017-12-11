(defun file-to-list (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun reduce-indexed (some_function some_list)
  (let ((x -1))
    (reduce (lambda(total item)
      (setf x (+ x 1))
      (apply some_function (list total item x))) some_list :initial-value 0)))

(defun list-from-string (some_string)
  (loop for some_char in (coerce some_string 'list)
    collecting (string some_char)))

(defun list-item-reoccurrence (some_item some_list)
  (reduce 
    (lambda (total item) (if (equal some_item item) (+ total 1) total))
    some_list
    :initial-value 0))

