;Common Lisp : A Gentle Introduction to Symbolic Computation
(defun square (x)
  "Returns x to the power of two"
  (* x x))

(defun onemorep (a b)
  "True if a is one more than b"
  (if (= a (+ 1 b)) t nil))

(defun cube (x)
  "Returns x to the power of 3"
  (* x x x))

(defun half (x)
  "Returns x divided by two"
  (/ x 2))

(defun pythag (x y)
  "Takes x and y as inputs and returns the square root of x^2 + y^2"
  (sqrt (+ (square x) (square y)))) 