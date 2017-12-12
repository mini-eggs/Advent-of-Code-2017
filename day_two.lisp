;;;;;;;;;;;;;;;;;;;;
;; ADVENT OF CODE ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; COMPLETE ;;
;;;;;;;;;;;;;;

;;;;;;;;;
;; DEPS;;
;;;;;;;;;

(load "./advent_of_code_core.lisp")
(load "~/quicklisp/setup.lisp")

(ql:quickload :cl-utilities)

;;;;;;;;;;;;;;
;; PART ONE ;;
;;;;;;;;;;;;;;

(defun calculate_spreadsheet_checksum (some_spreadsheet)
  (reduce 
    (lambda (total row) 
      (+ total (- (reduce #'max row) (reduce #'min row))))
    some_spreadsheet
    :initial-value 0))

;;;;;;;;;;;;;;
;; PART TWO ;;
;;;;;;;;;;;;;;

(defun calculate_evenly_divisible_number (some_list)
  (reduce 
    (lambda (outer_total current) 
      (reduce
        (lambda (inner_total item) 
          (if (equal (type-of (/ current item)) (type-of (/ 1 2)))
            inner_total
            (/ current item)))
        (remove current some_list)
        :initial-value outer_total))
    some_list
    :initial-value 0))

(defun calculate_spreadsheet_checksum_complex (some_spreadsheet)
  (reduce 
    (lambda (total row) 
      (+ total (calculate_evenly_divisible_number row)))
    some_spreadsheet
    :initial-value 0))

(defun calculate-answer (to_complete data_file)
  (apply to_complete (list (loop for line in (file-to-list data_file) 
    collect (loop for char_string in (cl-utilities:split-sequence #\Tab line)
      collect (parse-integer char_string))))))

;;;;;;;;;;;
;; OUTPUT;;
;;;;;;;;;;;

(format t "Part one: ")
(print (calculate-answer #'calculate_spreadsheet_checksum "day_two.txt"))
(format t "~C~C~C~C" #\return #\linefeed #\return #\linefeed)

(format t "Part two: ")
(print (calculate-answer #'calculate_spreadsheet_checksum_complex "day_two.txt"))
(format t "~C~C~C~C" #\return #\linefeed #\return #\linefeed)
