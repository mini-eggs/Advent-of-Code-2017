;;;;;;;;;;;;;;;;;;;;
;; ADVENT OF CODE ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; COMPLETE ;;
;;;;;;;;;;;;;;

;;;;;;;;;
;; DEPS;;
;;;;;;;;;

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-utilities)

(load "./advent_of_code_core.lisp")

;;;;;;;;;;;;;;
;; PART ONE ;;
;;;;;;;;;;;;;;

(defun list-is-unique (some_list)
  (equal (list-length some_list)
         (list-length (remove-duplicates some_list :test #'equal))))

(defun part-one-validate (some_list)
  (list-is-unique some_list))

;;;;;;;;;;;;;;
;; PART TWO ;;
;;;;;;;;;;;;;;

(defun string-sort (some_string)
  (sort some_string #'char-lessp))

(defun list-contains-amount (some_value some_list)
  (reduce
    (lambda (total item)
      (if (equal some_value item)
        (+ total 1)
        total))
    some_list
    :initial-value 0))

(defun list-has-anagrams (some_list)
  (reduce
    (lambda (total item)
      (let ((sorted_string (string-sort item)))
        (let ((sorted_list (map 'list #'string-sort some_list)))
          (let ((anagram_count (list-contains-amount sorted_string sorted_list)))
            (if (> anagram_count 1) t total)))))
    some_list
    :initial-value nil))

(defun part-two-validate (some_list)
  (and (part-one-validate some_list)
       (not (list-has-anagrams some_list))))

(defun file-to-answer (some_function some_file)
  (reduce
    (lambda (some_count some_line)
      (if (apply some_function (list (cl-utilities:split-sequence #\Space some_line)))
        (+ some_count 1)
        some_count))
    (file-to-list some_file)
    :initial-value 0))

;;;;;;;;;;;
;; OUTPUT;;
;;;;;;;;;;;

(format t "Part one:")
(print (file-to-answer #'part-one-validate "day_four.txt"))
(format t "~C~C~C~C" #\return #\linefeed #\return #\linefeed)

(format t "Part two:")
(print (file-to-answer #'part-two-validate "day_four.txt"))
(format t "~C~C~C~C" #\return #\linefeed #\return #\linefeed)
