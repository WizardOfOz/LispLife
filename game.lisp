; ======================================================================
; Conway's Game of Life
;   [game.lisp]
;
; by Mikko Saarela, 2011
; ======================================================================

(defparameter *live-cell* 1)
(defparameter *dead-cell* 0)
(defparameter *grid-size-x* 20)
(defparameter *grid-size-y* 20)
(defparameter *grid-array* nil)
(defparameter *live-cell-char* "*")
(defparameter *dead-cell-char* ".")

(defun iterate-cells (grid func)
  (dotimes (i *grid-size-y*)
    (let ((row (aref grid i)))
      (dotimes (j *grid-size-x*)
        (setf (aref row j) (funcall func))))))
 
(defun init-grid ()
  (defparameter *grid-array* (make-array  *grid-size-y*))
  (dotimes (i *grid-size-y*)
    (setf (aref *grid-array* i) (make-array  *grid-size-x*)))
  (iterate-cells *grid-array* (lambda () '0)))
    
(defun set-cell (x y type)
  (if (valid-cell-p x y)
    (setf (aref (aref *grid-array* y) x) type)))
  
(defun get-cell (x y)
  (if (valid-cell-p x y)
    (aref (aref *grid-array* y) x)))

(defun apply-deltas (deltas)
  (loop for delta in deltas do
    (set-cell (first delta) (second delta) (third delta))))

(defun tick ()
  (let ((deltas ()))
    (dotimes (y *grid-size-y*)
      (dotimes (x *grid-size-x*)
        (let ((num (num-neighbors x y))
              (cell (get-cell x y)))
          
          ; cell is live
          (if (eq *live-cell* cell)
            (if (or (eq 2 num)
                    (eq 3 num))
                ()
            (progn (print "death")
                   (setf deltas (cons (list x y 0) deltas)))))
          
          ; cell is dead
          (if (eq *dead-cell* cell)
            (if (eq 3 num)
              (progn
                (print "birth")
                (setf deltas (cons (list x y 1) deltas))))))))
          
          deltas))
      
(defun randomize-grid (grid)
  (iterate-cells grid #'randomize-cell))
 
(defun randomize-cell ()
  (random 2))

(defun print-cell (type)
  (princ (cond
            ((eq type *live-cell*) *live-cell-char*)
            ((eq type *dead-cell*) *dead-cell-char*))))

(defun print-grid ()
  (dotimes (y *grid-size-y*)
    (fresh-line)
    (dotimes (x *grid-size-x*)
      (print-cell (get-cell x y))))) 

(defun valid-cell-p (x y)
  (if (and (>= x 0)
           (< x *grid-size-x*)
           (>= y 0)
           (< y *grid-size-y*))
      t))

(defun num-neighbors (x y)
  (let ((num 0))
    ; top row
    (if (eq 1 (get-cell (1- x) (1- y))) (incf num))
    (if (eq 1 (get-cell x (1- y))) (incf num))
    (if (eq 1 (get-cell (1+ x) (1- y))) (incf num))
    ; middle row
    (if (eq 1 (get-cell (1- x) y)) (incf num))
    (if (eq 1 (get-cell (1+ x) y)) (incf num))
    ; bottom row
    (if (eq 1 (get-cell (1- x) (1+ y))) (incf num))
    (if (eq 1 (get-cell x (1+ y))) (incf num))
    (if (eq 1 (get-cell (1+ x) (1+ y))) (incf num))
    num))

; main program
(init-grid)
;(randomize-grid *grid-array*)
(print-grid)
