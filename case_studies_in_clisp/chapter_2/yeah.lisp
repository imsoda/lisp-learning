(defun power (x n)
  "Copied from the text book answer."
  (cond ((= n 0) 1)
	((evenp n) (expt (power x (/ n 2)) 2))
	(t (* x (power x (- n 1))))))

(defun count-atoms (expression)
  "Counts the number of atoms in an expression.
  The nil counts as a non-atom."
  (cond ((null expression) 0)
	((listp expression)
	 (+ (count-atoms (car expression))
	    (count-atoms (cdr expression))))
	(t 1)))

(defun count-all-atoms (expr)
  "Counts the number of atoms in an expression,
  counting nil as an atom only in non-tail position."
  (labels ((counts (expr if-nil)
	     (cond ((null expr) if-nil)
		   ((atom expr) 1)
		   (t (+ (counts (car expr) 1)
			 (counts (cdr expr) 0))))))
    (counts expr 0)))

(defun count-anywhere (sub-expr expr)
  "Counts the number of times the sub-expr occurs
  anywhere whithin the expr expression."
  (cond ((eql sub-expr expr) 1)
	((atom expr) 0)
	(t (+ (count-anywhere sub-expr (car expr))
	      (count-anywhere sub-expr (cdr expr))))))

(defun dot-product (seq-a seq-b)
  "Comput the dot product of the two sequences of numbers,
  i.e. the seq-a and seq-b."
  (apply #'+ (mapcar #'* seq-a seq-b)))

(defun dot-product-1 (a b)
  "Same to the previous function."
  (if (or (null a) (null b))
      0
      (+ (* (car a) (car b))
	 (dot-product-1 (cdr a) (cdr b)))))

(defun dot-product-2 (a b)
  "Same to the previous function."
  (let ((sum 0))
    (dotimes (i (length a))
      (incf sum (* (elt a i) (elt b i))))
    sum))