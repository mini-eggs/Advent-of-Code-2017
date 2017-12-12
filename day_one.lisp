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

;;;;;;;;;;;;;;
;; PART ONE ;;
;;;;;;;;;;;;;;

(defun list-of-chars-to-list-of-ints (some_list)
  (map 'list (lambda (some_char) (parse-integer some_char)) some_list))

(defun solve-captcha (some_list)
  (let ((first_item (nth 0 some_list)))
    (reduce-indexed
      (lambda (total current_item index)
        (let ((next_item (nth (+ index 1) some_list)))
          (if next_item
            (if (equal current_item next_item)
              (+ total current_item)
              total)
            (if (equal current_item first_item)
              (+ total current_item)
              total))))
      some_list)))

;;;;;;;;;;;;;;
;; PART TWO ;;
;;;;;;;;;;;;;;

(defun solve-captcha-complex (some_list)
  (let ((first_item (nth 0 some_list)))
    (reduce-indexed
      (lambda (total current_item index)
        (let ((next_item (nth (+ index (/ (list-length some_list) 2)) (append some_list some_list))))
            (if (equal current_item next_item)
              (+ total current_item)
              total)))
      some_list)))

;;;;;;;;;;;
;; OUTPUT;;
;;;;;;;;;;;

(format t "Part one:")
(loop for item in (file-to-list "day_one.txt")
  do (print (solve-captcha (list-of-chars-to-list-of-ints (list-from-string item)))))
(format t "~C~C~C~C" #\return #\linefeed #\return #\linefeed)

(format t "Part one:")
(loop for item in (file-to-list "day_one.txt")
  do (print (solve-captcha-complex (list-of-chars-to-list-of-ints (list-from-string item)))))
(format t "~C~C~C~C" #\return #\linefeed #\return #\linefeed)
