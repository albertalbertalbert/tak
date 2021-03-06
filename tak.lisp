;;;; Variable definitions
(defparameter *board*
  (list '(W W) '(Y W) nil nil '(B B B)
        '(W W) '(W W) '(W W B) nil nil
        nil '(W) '(W)  '(W B W)  nil
        nil nil nil '(W)  '(W W) 
        nil nil nil nil nil ));;sample for testing
(defvar *side* '(WHITE BLACK))
(defvar *pieces* '(road blocker capstone))
(defparameter *piece-map* (make-hash-table :test 'equal))
(setf (gethash '(road white) *piece-map*) 'W)
(setf (gethash '(road black) *piece-map*) 'B)
(setf (gethash '(blocker white) *piece-map*) 'X)
(setf (gethash '(blocker black) *piece-map*) 'C)
(setf (gethash '(capstone white) *piece-map*) 'Y)
(setf (gethash '(capstone black) *piece-map*) 'Z)



(defvar *last-col* (list 4 9 14 19 24))
(defvar *start* (list '(0) '(5) ))

(defvar *directions* (list 'up 'down 'left 'right))

(defvar *header* "----------------------------------------------")
(defvar *separator* "----------------------------------------------")
(defvar *footer* "----------------------------------------------")
;;;; Variable definition complete

(defun initialize-tak-board ()
  (list nil nil nil nil nil
        nil nil nil nil nil
        nil nil nil nil nil
        nil nil nil nil nil
        nil nil nil nil nil))

(defun print-board (board)
  (format t "~%~a~%" *header*)
  (dotimes (i 5)
    (let ((line (subseq board (* i 5) (+ 5 (* i 5)))))
      (format t "~{|~a~1,9@T~}|~%" line)
      (format t "~a~%" *separator*))))

(defun evaluate (moves)
  (first moves))

(defun adjacent (cell direction)
  (if (or (< cell 0) (> cell 24)) (error "Invalid cell!"))
  (ecase direction
    (up (and (> cell 4) (- cell 5)))
    (down (and (< cell 20) (+ cell 5)))
    ((right) (and (/= 4 (mod cell 5)) (+ 1 cell)))
    ((left) (and (/= 0 (mod cell 5)) (- cell 1)))))

(defun next-cell (cell board path)
  (let ((next (remove-if #'null (mapcar (lambda (x) (adjacent cell x) ) *directions*) )))
    (setf next (remove-if #'null (remove-if-not (lambda (x) (eq (first (nth x board)) (first (nth cell board)))) next)))
    (mapcar (lambda (x) (if (member x path)
                            path
                          (cons x path))) next)))

(defun new-paths (board paths)
  (remove-duplicates
   (mapcan (lambda (x) (next-cell (first x) board x)) paths)
   :test #'equal))


(defun fixed-point (arg1 arg2)
  (let ((result (new-paths arg1 arg2)))
    (if (equal arg2 result)
        arg2
      (fixed-point arg1 result))))

(defun winning (board)
  (mapcan (lambda (y)
            (remove-if-not 
             (lambda (x) (member y x) )
             (fixed-point board *start*)))
          *last-col*))

(defun moves (cell board side)
  (when (null (member side *side*))
    (error "Invalid side"))
  (cond
   ((null (nth cell board))
    (mapcar (lambda (x) (add-piece cell x board side)) *pieces*))))

(defun add-piece (cell piece board side)
  (when (or (< cell 0) (> cell 24)) (error "Cell must be between 0-24"))
  (let ((before-cell nil)
        (after-cell nil)
        (active-cell (nth cell board))
        (new-piece nil) )
    (cond
     ((= 0 cell) 
      (setf after-cell (rest board)))
     ((= 24 cell)
      (setf before-cell (subseq board 0 cell )))
     (t 
      (setf before-cell (subseq board 0 cell))
      (setf after-cell (subseq board (+ cell 1) 25))))

    (setf new-piece (gethash (list piece side) *piece-map*))
    (when (null new-piece)
      (error "Could not determine type of new piece"))

    (concatenate 'list before-cell (list (push new-piece active-cell)) after-cell)))

(defun move-stack (cell board stack  dir)
  (let ((stack-color 'W)
        (new-cell (adjacent cell dir))
        (if (or (eq stack-color ))))
  
  
  
    )





