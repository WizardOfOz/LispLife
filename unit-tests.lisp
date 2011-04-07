; tests here
 
(defun test-shape ()
	(setf (aref (aref *cell-row-array* 10) 11) 1)
	(setf (aref (aref *cell-row-array* 11) 9) 1)
	(setf (aref (aref *cell-row-array* 10) 10) 1)
	(setf (aref (aref *cell-row-array* 11) 10) 1)
	(setf (aref (aref *cell-row-array* 12) 10) 1))
