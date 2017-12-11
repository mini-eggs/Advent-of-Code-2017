;;;;;;;;;;;;;;;;;;;;
;; ADVENT OF CODE ;;
;;;;;;;;;;;;;;;;;;;;

(load "~/quicklisp/setup.lisp")
(load "advent_of_code_core.lisp")

(ql:quickload :cl-utilities)
(ql:quickload :alexandria)

(defun construct-hash (list_of_instructions)
  (let ((instruction_hash (make-hash-table :test 'equal)))
    (loop for instruction in list_of_instructions do
      (let ((elements (cl-utilities:split-sequence #\Space instruction)))
        (setf (gethash (nth 0 elements) instruction_hash) 0)
        (setf (gethash (nth 4 elements) instruction_hash) 0)))
    instruction_hash))


(defclass simple_instruction ()
  ( (registry :accessor registry :initarg :registry)                            ; string name
    (instruction_func :accessor instruction_func :initarg :instruction_func)    ; function of instruction: inc/dec
    (instruction_cond :accessor instruction_cond :initarg :instruction_cond)))  ; condition if the instruct should run or not

(defun get-instruction (some_instruction)
  (let ((elements (cl-utilities:split-sequence #\Space some_instruction)))
    (make-instance 'simple_instruction  
      :registry (nth 0 elements)                                  
      :instruction_func (if (equal (nth 1 elements) "inc")
                          (lambda (some_amount) (+ some_amount (parse-integer (nth 2 elements))))
                          (lambda (some_amount) (- some_amount (parse-integer (nth 2 elements)))))
      :instruction_cond (lambda (some_hash) 
                          (let ((some_type (nth 5 elements)))
                            (let ((some_value (gethash (nth 4 elements) some_hash)))
                              (let ((some_modifier (parse-integer (nth 6 elements))))
                                (let ((next_value nil))
                                  (if (equal ">" some_type)
                                    (setf next_value (> some_value some_modifier)))
                                  (if (equal ">=" some_type)
                                    (setf next_value (>= some_value some_modifier)))
                                  (if (equal "<" some_type)
                                    (setf next_value (< some_value some_modifier)))
                                  (if (equal "<=" some_type)
                                    (setf next_value (<= some_value some_modifier)))
                                  (if (equal "!=" some_type)
                                    (setf next_value (not (equal some_value some_modifier))))
                                  (if (equal "==" some_type)
                                    (setf next_value (equal some_value some_modifier)))
                                  next_value)))))
    )))

(defun run-instructions (list_of_instructions some_hash)
  (loop for string_instruction in list_of_instructions do
    (let ((current_instruction (get-instruction string_instruction)))
      (if (apply (instruction_cond current_instruction) (list some_hash))
        (setf (gethash (registry current_instruction) some_hash) (apply (instruction_func current_instruction) (list (gethash (registry current_instruction) some_hash)))))))
  some_hash)

(defun highest-value-of-registries (some_hash)
  (let ((hash_values (loop for some_key in (alexandria:hash-table-keys some_hash) collect (gethash some_key some_hash))))
    (nth 0 (sort hash_values #'>))))

(let ((instructions (loop for line in (file-to-list "day_eight.txt") collect line)))
  (let ((registries (construct-hash instructions)))
    (print "Part one:")
    (print (highest-value-of-registries (run-instructions instructions registries)))
    (format t "~C~C" #\return #\linefeed)))
