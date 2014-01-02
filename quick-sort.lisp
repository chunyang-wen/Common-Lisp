(defun quick-sort (arr start end)
 "Quick sort body"
 (if (< start end)
  (let ((n-pair (partition arr start end)))
   (quick-sort arr start (car n-pair))
   (quick-sort arr (cdr n-pair) end))
  ))

(defun partition (arr start end)
 "Partition according to pivot."
 (let ((pivot (aref arr start)) (cur start))
  (loop while (<= start end) do
   (cond
	((< pivot (aref arr start)) ; pivot < arr[start], swap with arr[end]
	 (swap arr start end) (decf end))
	((> pivot (aref arr start)) ; pivot > arr[start], swap with arr[start]
	 (swap arr cur start) (incf cur) (incf start))
	(t                          ; otherwise
	 (incf start))))
  (cons (decf cur) start))) 
  
(defun swap (arr i j)
 "Swap element of arr"
 (let ((tmp (aref arr i)))
  (setf (aref arr i) (aref arr j))
  (setf (aref arr j) tmp)))


