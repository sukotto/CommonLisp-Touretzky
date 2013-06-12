(defun make-even (x)
 "Makes an odd number even by adding one to it."
 (if (oddp x)(+ 1 x) x))

(defun further (x)
 "Adds one to positive number, subtracts one from negative number"
 (cond ((> x 0) (+ 1 x))
       ((< x 0) (- x 1))
       (t x)))

(defun my-not (x)
 (if x nil t))

;Not a macro - evaluates all arguments
(defun nor (a b)
  (not (or a b)))

(defun logical-and (x y) (and x y t))

