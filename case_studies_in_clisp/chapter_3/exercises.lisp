(defun e3.1 ()
  (= (let* ((x 6)
	    (y (* x x)))
       (+ x y))
     ((lambda (x)
	((lambda (x y)
	   (+ x y))
	 x
	 (* x x)))
      6)))

(defun dprint (x)
  (cond ((atom x) (princ x))
	(t (princ "(")
	   (dprint (first x))
	   (princ " . ")
	   (dprint (rest x))
	   (princ ")")
	   x)))

(defun dp (x)
  (cond ((atom x) (princ x))
	(t (princ "(")
	   (dp (first x))
	   (pr-rest (rest x))
	   (princ ")")
	   x)))

(defun pr-rest (x)
  (cond ((null x))
	((atom x) (princ " . ") (princ x))
	(t (princ " ") (dp (first x)) (pr-rest (rest x)))))

(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

(defun find-all (item sequence &rest keyword-args
		 &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
	     :test (complement test) keyword-args)))

(defun find-all-rkey (item sequence &rest keyword-args
		      &key (test #'eql) test-not &allow-other-keys)
  "'rkey' stands for reversly search the value for the key."
  (labels ((getf-from-end (key seq)
	     (let ((index (+ 1 (position key seq :from-end t))))
	       (if index
		   (elt seq index)
		   #'eql))))
    (if test-not
	(apply #'remove item sequence
	       :test-not (complement
			  (getf-from-end :test-not keyword-args))
	       keyword-args)
	(apply #'remove item sequence
	       :test (complement
		      (getf-from-end :test keyword-args))
	       keyword-args))))