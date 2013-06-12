;; Chapter 9 Input/Output
(defun saying()
  (format t "~&There are old pilots,")
  (format t "~&and there are bold pilots,")
  (format t "~&but there are no old bold pilots."))

(defun draw-line(n)
 "Draws a line of asterisks of length n"
 (if (> n 0) (progn (format t "*")
                    (draw-line (1- n)))
             (format t "~%")))

(defun draw-box (width height)
 "Draws a box with the specified width and height"
  (when (> height 0)(progn (draw-line width)
                         (draw-box width (1- height)))))

(defun ninety-nine-bottles (n)
 "Sings the ninety-nine bottles of beer song from n to 0"
 (if (> n 0)(progn (format t "~&~S bottles of beer on the wall," n)
                   (format t "~&~S bottles of beer!" n)
                   (format t "~&Take one down,")
                   (format t "~&Pass it around,")
                   (format t "~&~S bottles of beer on the wall." (1- n))
                   (ninety-nine-bottles (1- n)))
            (format t "~&No more bottles of beer on the wall!~%")))

;;Keyboard exercise - graph of an arbitrary function
(defun space-over(n)
 "Moves the cursor to the right by printing n spaces"
 (cond ((< n 0) (format t "~&Error~%!"))
       ((> n 2) (progn (format t " ")
                       (space-over (1- n))))
       (t (format t " "))))

(defun test(n)
  (format t "~%>>>")
  (space-over n) 
  (format t "<<<"))

(defun plot-one-point (plotting-string y-val)
 "Prints plotting-string in column y-val"
  (space-over y-val)
  (format t "~A~%" plotting-string))

(defun plot-points (plotting-string y-values)
 "Plots a list of y-values as plotting-string"
 (mapcar (lambda(y)(plot-one-point plotting-string y)) y-values)
 NIL)

(defun generate (m n)
 "Returns a list of integers from m to n"
  (labels((aux (m acc)
            (if (> m n) (reverse acc)
                (aux (1+ m) (cons m acc)))))
  (aux m '())))

(defun make-graph()
  "Prompts for values and plots a graph"
  (format t "~&Function to graph?")
  (let((fn (read)))
    (format t "~&Starting x value?")
    (let((start-x (read)))
      (format t "~&Ending x value?")
      (let((end-x (read)))
          (format t "~&Plotting string?")
          (let*((plotting-string (read))
                (x-values (generate start-x end-x))
                (y-values (mapcar (lambda(x)(funcall fn x)) x-values)))
               (plot-points plotting-string y-values))))))

(defun square(x)
  (* x x))