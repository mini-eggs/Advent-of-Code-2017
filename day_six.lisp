;;;;;;;;;;;;;;;;;;;;
;; ADVENT OF CODE ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; DEPS ;;
;;;;;;;;;;

(load "./advent_of_code_core.lisp")
(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-utilities)

;;;;;;;;;;;;;;;;;;
;; PART ONE/TWO ;;
;;;;;;;;;;;;;;;;;;

(defclass cycle ()
  ((cycles :accessor cycles :initarg :cycles)
    (redistribution_cycles :accessor redistribution_cycles :initarg :redistribution_cycles)))

(defun get-highest-value-index (some_memory_bank)
  (reduce-indexed 
    (lambda (total item index) 
      (if (equal (reduce #'max some_memory_bank) item)
        (- (list-length some_memory_bank) index 1)
        total)) 
    (reverse some_memory_bank)))

(defun get-next-memory-bank-position (some_position some_memory_bank)
  (let ((next_position (+ some_position 1)))
    (if (>= next_position (list-length some_memory_bank)) 0 next_position)))

(defun organize-memory-bank (some_memory_bank)
  "Get next memory bank after one iteration."
  (let ((highest_value_index (get-highest-value-index some_memory_bank)))
    (let ((current_position highest_value_index))
      (let ((current_memory_bank some_memory_bank))
        (let ((memory_credits (nth highest_value_index current_memory_bank)))
          (setf (nth current_position current_memory_bank) 0)
          (loop while (> memory_credits 0) do
            (let ((next_position (get-next-memory-bank-position current_position some_memory_bank)))
              (setf (nth next_position current_memory_bank) (+ (nth next_position current_memory_bank) 1))
              (setf current_position next_position)
              (setf memory_credits (- memory_credits 1))))
          current_memory_bank)))))

(defun get-hash-key-from-memory-bank (some_memory_bank)
  (let ((key "_"))
    (loop for item in some_memory_bank
      do (setf key (concatenate 'string key (write-to-string item) "_")))
    key))
          
(defun calculate-cycles-before-identical (some_memory_bank)
  (let ((previous_states (make-hash-table :test 'equal)))
    (let ((cycle_count 0))
      (let ((redistcibution_cycle_count 0))
        (let ((state_has_been_seen nil))
          (let ((compare_memory_bank (copy-list some_memory_bank)))
            (let ((current_memory_bank (copy-list some_memory_bank)))
              (loop while (not state_has_been_seen) do
                ;; get next memory bank
                (setf current_memory_bank (organize-memory-bank current_memory_bank))
                ;; get hash key
                (let ((hash_key (get-hash-key-from-memory-bank current_memory_bank)))
                  ;; increment our loop count
                  (setf cycle_count (+ cycle_count 1))
                  ;; check if this state has been seen before
                  (if (gethash hash_key previous_states)
                    (progn
                      (setf state_has_been_seen t)
                      (setf redistcibution_cycle_count (- cycle_count (gethash hash_key previous_states)))))
                  ;; push state into hash
                  (setf (gethash hash_key previous_states) cycle_count)))
              (make-instance 'cycle :cycles cycle_count :redistribution_cycles redistcibution_cycle_count))))))))

;;;;;;;;;;;;
;; OUTPUT ;;
;;;;;;;;;;;;

(loop for item in (file-to-list "day_six.txt")
  do (let ((cycle_values (calculate-cycles-before-identical (map 'list (lambda (item) (parse-integer item)) (cl-utilities:split-sequence #\Tab item)))))
    (format t "Part one:")
    (print (cycles cycle_values))
    (format t "~C~C~C~C" #\return #\linefeed #\return #\linefeed)
    (format t "Part two:")
    (print (redistribution_cycles cycle_values))
    (format t "~C~C~C~C" #\return #\linefeed #\return #\linefeed)))
