;;;;;;;;;;;;;;;;;;;;
;; ADVENT OF CODE ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; COMPLETE ;;
;;;;;;;;;;;;;;

(load "~/quicklisp/setup.lisp")
(load "./advent_of_code_core.lisp")

(ql:quickload :cl-utilities)
(ql:quickload :alexandria)

(defun construct-hash (list_of_instructions)
  (reduce (lambda (some_hash some_instruction) 
            (let ((elements (cl-utilities:split-sequence #\Space some_instruction)))
              (setf (gethash (nth 0 elements) some_hash) 0)
              (setf (gethash (nth 5 elements) some_hash) 0)
              some_hash))
          list_of_instructions
          :initial-value (make-hash-table :test 'equal)))

(defclass simple_instruction ()
  ( (registry :accessor registry :initarg :registry)                              ; string name
    (instruction_func :accessor instruction_func :initarg :instruction_func)      ; function of instruction: inc/dec
    (instruction_cond :accessor instruction_cond :initarg :instruction_cond)))    ; condition if the instruct should run or not

(defclass registry_data ()
  ( (current-max-data :accessor current-max-data :initarg :current-max-data)      ; max value at any point in the registry instruction cycle
    (absolute-max-data :accessor absolute-max-data :initarg :absolute-max-data))) ; max value that appears in registry during it's life

(defun get-instruction (some_instruction)
  (let ((elements (cl-utilities:split-sequence #\Space some_instruction)))
    (make-instance 'simple_instruction  
      :registry (nth 0 elements)                                  
      :instruction_func (let ((some_function (if (equal (nth 1 elements) "inc") #'+ #'-)))
                          (lambda (some_amount) (apply some_function (list some_amount (parse-integer (nth 2 elements))))))
      :instruction_cond (lambda (some_hash)
                          (let ((some_type (nth 5 elements)))
                            (let ((some_value (gethash (nth 4 elements) some_hash)))
                              (let ((some_modifier (parse-integer (nth 6 elements))))
                                (cond ((equal some_type ">") (> some_value some_modifier))
                                      ((equal some_type ">=") (>= some_value some_modifier))
                                      ((equal some_type "<") (< some_value some_modifier))
                                      ((equal some_type "<=") (<= some_value some_modifier))
                                      ((equal some_type "!=") (not (equal some_value some_modifier)))
                                      ((equal some_type "==") (equal some_value some_modifier))))))))))

(defun get-current-max (some_hash)
  (reduce (lambda (total item)
            (let ((value (gethash item some_hash)))
              (if (> value total) value total)))
          (alexandria:hash-table-keys some_hash)
          :initial-value 0))

(defun run-instructions (list_of_instructions some_hash)
  (let ((absolute_max 0))
    ;;run all instructions
    (loop for string_instruction in list_of_instructions do
      (let ((current_instruction (get-instruction string_instruction)))
        (if (apply (instruction_cond current_instruction) (list some_hash))
          (let ((next_value (apply (instruction_func current_instruction) (list (gethash (registry current_instruction) some_hash)))))
            (if (> next_value absolute_max) (setf absolute_max next_value))
            (setf (gethash (registry current_instruction) some_hash) next_value)))))
    ;; calculate current max and 
    ;; return registry data instance
    (make-instance 'registry_data :absolute-max-data absolute_max
                                  :current-max-data (get-current-max some_hash))))

(defun main ()
  (let ((instructions (loop for line in (file-to-list "day_eight.txt") collect line)))
    (let ((registries (construct-hash instructions)))
      (let ((registry_instance (run-instructions instructions registries)))
        (print "Part one:")
        (print (current-max-data registry_instance))
        (format t "~C~C" #\return #\linefeed)
        (print "Part two:")
        (print (absolute-max-data registry_instance))
        (format t "~C~C~C~C" #\return #\linefeed #\return #\linefeed)))))

(main)