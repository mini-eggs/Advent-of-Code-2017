;;;;;;;;;;;;;;;;;;;;
;; ADVENT OF CODE ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; COMPLETE ;;
;;;;;;;;;;;;;;

(load "./advent_of_code_core.lisp")

(defparameter test_input (list 0 3 0 1 -3))

(defun incrementor-part-one (offset_int) 
  (+ offset_int 1))

(defun incrementor-part-two (offset_int)
  (+ offset_int (if (>= offset_int 3) -1 1)))

(defun calculate-number-of-steps (incrementor_function some_list)
  (let ((out_of_bounds nil))
    (let ((current_position 0))
      (let ((number_of_steps 0))
        (loop while (not out_of_bounds) do
          (let ((increment (nth current_position some_list)))
            ;; check if we should stop
            (if (not increment)
              (setf out_of_bounds t))
            ;; increment values, business as usual
            (if increment
              ((lambda ()
                  (setf (nth current_position some_list) (apply incrementor_function (list increment)))
                  (setf current_position (+ current_position increment))
                  (setf number_of_steps (+ number_of_steps 1)))))))
        number_of_steps))))

;;;;;;;;;;;;
;; OUTPUT ;;
;;;;;;;;;;;;

(format t "Part one")
(print (calculate-number-of-steps #'incrementor-part-one
  (loop for line in (file-to-list "day_five.txt")
    collect (parse-integer line))))
(format t "~C~C" #\return #\newline)

(format t "Part two")
(print (calculate-number-of-steps #'incrementor-part-two
  (loop for line in (file-to-list "day_five.txt")
    collect (parse-integer line))))
(format t "~C~C" #\return #\newline)


