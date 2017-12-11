;;;;;;;;;;;;;;;;;;;;
;; ADVENT OF CODE ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; COMPLETE ;;
;;;;;;;;;;;;;;

;;;;;;;;;;;
;; INPUT ;;
;;;;;;;;;;;

(defparameter puzzle_input 312051)

;;;;;;;;;;;;;;
;; PART ONE ;;
;;;;;;;;;;;;;;

(defun calculate-steps-part-one (some_num)
  (let ((root (floor (sqrt some_num))))
    (let ((length_of_current_side (if (not (equal (mod root 2) 0)) root (+ root 1) )))
      (let ((length_of_axis_to_center (/ (- length_of_current_side 1) 2)))
        (let ((cycle (- some_num (* (- length_of_current_side 2) (- length_of_current_side 2)))))
          (let ((some_offset (mod cycle (- length_of_current_side 1))))
            (+ length_of_axis_to_center (abs (- some_offset length_of_axis_to_center)))))))))

;;;;;;;;;;;;;;
;; PART TWO ;;
;;;;;;;;;;;;;;

(defclass direction ()
  ((x :accessor x :initarg :x)
    (y :accessor y :initarg :y)))

(defun construct-direction-list (top_num)
  (let ((right (make-instance 'direction :x 1 :y 0)))
    (let ((up (make-instance 'direction :x 0 :y 1)))
      (let ((down (make-instance 'direction :x -1 :y 0)))
        (let ((left (make-instance 'direction :x 0 :y -1 )))
          (let ((list_of_directions (list right up down left)))
            (let ((list_of_final_directions (list )))
              (let ((amount_of_times_to_add 0))
                (let ((every_x_loop_update 0))
                  (let ((loop_iterator 0))
                    (let ((loop_iterator_final 0))
                      (let ((update_next_loop t))
                        (loop while (< (list-length list_of_final_directions) top_num) do  
                          ;; add current items to final list returned 
                          (loop for _ from 0 to amount_of_times_to_add
                                for next_item = (list (nth (mod loop_iterator_final (list-length list_of_directions)) list_of_directions))
                                collect (setf list_of_final_directions (append list_of_final_directions next_item)))
                          ;; determine what we should do next loop    
                          (if (>= loop_iterator every_x_loop_update)
                            (if update_next_loop
                              (setf update_next_loop nil)
                              ((lambda ()
                                (setf update_next_loop t)
                                (setf amount_of_times_to_add (+ amount_of_times_to_add 1)))))
                            (setf loop_iterator 0))
                         ; update iterators
                         (setf loop_iterator (+ loop_iterator 1))
                         (setf loop_iterator_final (+ loop_iterator_final 1)))
                    list_of_final_directions))))))))))))

(defun coord-to-key (some_coord)
  (let ((x_string (write-to-string (+ (x some_coord) 0))))
    (let ((y_string (write-to-string (+ (y some_coord) 0))))
      (concatenate 'string x_string "," y_string))))

(defun coord-to-key-offset (x y some_coord)
  (let ((next_direction (make-instance 'direction :x (+ x (x some_coord)) :y (+ y (y some_coord)))))
    (coord-to-key next_direction)))
    
(defun get-next-number (some_coord some_spiral)
  (+  (gethash (coord-to-key-offset -1  1 some_coord) some_spiral 0)   ; top left
      (gethash (coord-to-key-offset  0  1 some_coord) some_spiral 0)   ; top center
      (gethash (coord-to-key-offset  1  1 some_coord) some_spiral 0)   ; top right
      (gethash (coord-to-key-offset -1  0 some_coord) some_spiral 0)   ; center left
      (gethash (coord-to-key-offset  0  0 some_coord) some_spiral 0)   ; center center
      (gethash (coord-to-key-offset  1  0 some_coord) some_spiral 0)   ; center right
      (gethash (coord-to-key-offset -1 -1 some_coord) some_spiral 0)   ; bottom left
      (gethash (coord-to-key-offset  0 -1 some_coord) some_spiral 0)   ; bottom bottom
      (gethash (coord-to-key-offset  1 -1 some_coord) some_spiral 0))) ; bottom right

(defun get-next-spiral-value (top_num)
  (let ((spiral))
    (let ((loop_count 0))
      (let ((x_current 0))
        (let ((y_current 0))
          (let ((num_current 1))
            (setq spiral (make-hash-table :test 'equal))
            (loop while (< num_current top_num) do
              (let ((pos_current (make-instance 'direction :x x_current :y y_current) ))
                (let ((next_number (get-next-number pos_current spiral)))
                  ;; set next number
                  (let ((next_number_safe (if (equal next_number 0) 1 next_number)))
                    (setf (gethash (coord-to-key pos_current) spiral) next_number_safe)
                    (setf num_current next_number_safe))
                  ;; update current postion
                  (let ((direction_addition (nth loop_count (construct-direction-list (+ loop_count 1)))))
                    (setf x_current (+ x_current (x direction_addition)))
                    (setf y_current (+ y_current (y direction_addition))))
                  ;; increment loop count and continue
                  (setf loop_count (+ loop_count 1)))))
            num_current))))))

;;;;;;;;;;;
;; OUTPUT;;
;;;;;;;;;;;

(format t "Part one: ")
(print (calculate-steps-part-one puzzle_input))
(format t "~C~C" #\newline #\linefeed)

(format t "Part one: ")
(print (get-next-spiral-value puzzle_input))
(format t "~C~C" #\newline #\linefeed)
