;;;;;;;;;;;;;;;;;;;;
;; ADVENT OF CODE ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; COMPLETE ;;
;;;;;;;;;;;;;;

;;;;;;;;;;
;; DEPS ;;
;;;;;;;;;;

(load "~/quicklisp/setup.lisp")
(load "./advent_of_code_core.lisp")
(ql:quickload :cl-utilities)

;;;;;;;;;;;;;;
;; PART ONE ;;
;;;;;;;;;;;;;;

(defclass tree_item ()
  ( (name :accessor name :initarg :name)
    (parent :accessor parent :initarg :parent)
    (weight :accessor weight :initarg :weight)
    (children :accessor children :initarg :children)))

(defun read-item-for-input (line_item)
  (let ((elements (cl-utilities:split-sequence #\Space line_item)))
    (let ((name (nth 0 elements)))
      (let ((weight (parse-integer (string-trim  (list #\( #\)) (nth 1 elements)))))
        (let ((children  (if (> (list-length elements) 3) (subseq elements 3 (list-length elements)) (list ))))
          (setf children (map 'list (lambda (item) (string-trim (list #\, ) item)) children))
          (make-instance 'tree_item :name name :weight weight :children children :parent nil))))))

(defun get-tree-item-instances (some_hash_key some_hash &optional parent_instance)
  "This function is really turning our `dumb` references from the above function
   into valid parent + child references (i.e. not a list of string for children
   but real class instances)."
  (let ((tree_data nil))
    ;; Start constructing our instance.
    (setf tree_data (make-instance 'tree_item 
                                    :name (name (gethash some_hash_key some_hash))
                                    :weight (weight (gethash some_hash_key some_hash))
                                    :children (list )))
    ;; If there is a parent instance hook it up
    ;; this will not be the case with our first (root) item.
    (if parent_instance (setf (parent tree_data) parent_instance))
    ;; Hook up our instances children.
    (loop for dumb_child in (children (gethash some_hash_key some_hash)) do
      (setf (children tree_data) (append (children tree_data) (list (get-tree-item-instances dumb_child some_hash tree_data)))))
    ;; return our current tree.
    tree_data))

(defun get-tree-structure (tree_items_list)
  (let ((root nil))
    (let ((final_tree nil))
      (let ((tree_items_hash (make-hash-table :test 'equal)))
        ;; push all tree items into hash
        (loop for item in tree_items_list do
          (setf (gethash (name item) tree_items_hash) item))
        ;; set every item's parent value
        (loop for item in tree_items_list do
          (loop for child in (children item) do
            (setf (parent (gethash child tree_items_hash)) (name item))))
        ;; find item which does not have parent
        (loop for item in tree_items_list do
          (if (not (parent item))
            (setf root item)))
        ;; construct new tree list with children being references rather than strings
        ;; along with parents being references, too.
        (setf final_tree (get-tree-item-instances (name root) tree_items_hash))
        ;; return tree structure
        final_tree))))

(defun get-leg-weight (some_leg)
  (reduce
    (lambda (total item) (+ total (get-leg-weight item)))
    (children some_leg)
    :initial-value (weight some_leg)))


(defun calculate-correct-weight (some_tree_structure some_weight)
  (let ((weight_offset (- (reduce (lambda (total item) (+ total (get-leg-weight item))) (children some_tree_structure) :initial-value (weight some_tree_structure)) some_weight)))
    (if (> weight_offset 0)
      (- (weight some_tree_structure) weight_offset)
      (+ (weight some_tree_structure) weight_offset))))

(defun get-corrected-weight (some_tree_structure &optional target_weight)
  (let ((loop_count 0))
    (let ((abnormal_weight_item nil))
      (let ((next_target_weight))
        (let ((current_weights (loop for child in (children some_tree_structure) collect (get-leg-weight child))))
          ;; find the different item
          ;; along with the target weight
          (loop for single_weight in current_weights do
            (if (equal (list-item-reoccurrence single_weight current_weights)  1)
              (setf abnormal_weight_item (nth loop_count (children some_tree_structure)))
              (setf next_target_weight single_weight))
            (setf loop_count (+ loop_count 1)))
          (if abnormal_weight_item
            (get-corrected-weight abnormal_weight_item next_target_weight)
            (calculate-correct-weight some_tree_structure target_weight)))))))

;;;;;;;;;;;;
;; OUTPUT ;;
;;;;;;;;;;;;

(let ((tree_structure (get-tree-structure (loop for item in (file-to-list "day_seven.txt") collect (read-item-for-input item)))))
  (format t "Day one:")
  (print (name tree_structure))
  (format t "~C~C" #\newline #\linefeed)
  (format t "Day two:")
  (print (get-corrected-weight tree_structure))
  (format t "~C~C" #\newline #\linefeed))
