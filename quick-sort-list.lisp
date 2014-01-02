(defun quick-sort (listin)
 (cond
  ((null listin) nil)
  (t
   (append
	(quick-sort (list< (car listin) (cdr listin)))
	(cons (car listin) nil)
	(quick-sort (list>= (car listin) (cdr listin)))))))

(defun list< (a b)
 (cond
  ((or (null a) (null b)) nil)
  ((< a (car b)) (list< a (cdr b)))
  (t (cons (car b) (list< a (cdr b))))))

(defun list>= (a b)
 (cond
  ((or (null a) (null b)) nil)
  ((>= a (car b)) (list>= a (cdr b)))
  (t (cons (car b) (list>= a (cdr b))))))

