; ======================================================================
; Conway's Game of Life
;   [unit-tests.lisp]
;
; by Mikko Saarela, 2011
; ======================================================================

(use-package :lisp-unit)

; fixture:
;     9 
; 10:  XX
; 11: XX
; 12:  X
(defun test-shape ()
  (setf (aref (aref *grid-array* 10) 11) 1)
  (setf (aref (aref *grid-array* 11) 9) 1)
  (setf (aref (aref *grid-array* 10) 10) 1)
  (setf (aref (aref *grid-array* 11) 10) 1)
  (setf (aref (aref *grid-array* 12) 10) 1))

;(defun iterate-cells (grid func)
; check that the func is called x * y times
(define-test iterate-cells
  (assert-equal 0 0))

;(defun init-grid ()
; tests only one line well
(define-test init-grid
  (assert-true *grid-array*)
  (assert-equal *grid-size-y* (length *grid-array*))
  (assert-equal *grid-size-x* (length (aref *grid-array* 0)))
  (assert-equal 0 (length (remove-if (lambda (x) (= x 0)) (aref *grid-array* 0)))))

;(defun set-cell (x y type)
; valid cell, invalid cell
(define-test set-cell
  (assert-equal 0 0))
    
;(defun get-cell (x y)
(define-test get-cell
  (assert-equal 0 0))

;(defun apply-deltas (deltas)
(define-test apply-deltas
  (assert-equal 0 0))

;(defun tick ()
(define-test tick
  (assert-equal 0 0))

;(defun randomize-grid (grid)
(define-test randomize-grid
  (assert-equal 0 (length (remove-if (lambda (x) (= x 0)) (aref *grid-array* 0)))))

;(defun randomize-cell ()
(define-test randomize-cell
  (assert-true (< (randomize-cell) 2)))

;(defun print-cell (type)
(define-test print-cell 
  (assert-equal *live-cell-char* (print-cell 1))
  (assert-equal *dead-cell-char* (print-cell 0)))

;(defun print-grid ()
; No need to test this!

;(defun valid-cell-p (x y)
(define-test valid-cell-p
  (assert-equal t (valid-cell-p 0 0))
  (assert-equal t (valid-cell-p (1- *grid-size-x*) (1- *grid-size-x*)))
  (assert-equal nil (valid-cell-p -1 0))
  (assert-equal nil (valid-cell-p 4 -3))
  (assert-equal nil (valid-cell-p *grid-size-x* *grid-size-x*)))
  
;(defun num-neighbors (x y)
(define-test num-neighbors-empty-grid
  (assert-equal 0 (num-neighbors 0 0))
  (assert-equal 0 (num-neighbors 1 1))
  (assert-equal 0 (num-neighbors 2 2)))

;(defun num-neighbors (x y)
(define-test num-neighbors-full-grid
  (assert-equal 3 (num-neighbors 9 10))
  (assert-equal 3 (num-neighbors 10 10))
  (assert-equal 2 (num-neighbors 11 10))

  (assert-equal 3 (num-neighbors 9 11))
  (assert-equal 4 (num-neighbors 10 11))
  (assert-equal 4 (num-neighbors 11 11))

  (assert-equal 3 (num-neighbors 9 12))
  (assert-equal 2 (num-neighbors 10 12))
  (assert-equal 2 (num-neighbors 11 12)))

; run
(run-tests
  init-grid
  iterate-cells
  set-cell
  get-cell
  apply-deltas
  randomize-grid
  randomize-cell
  num-neighbors-empty-grid
  print-cell
  valid-cell-p)

; run tests again
(test-shape)

(run-tests
  tick
  num-neighbors-full-grid)
  