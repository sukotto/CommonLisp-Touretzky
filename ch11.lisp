;ch8 has the definition of flatten
(load "ch8.lisp")

(defun it-length (l)
 "An iterative version of length. Returns the length of a list"
  (let ((count 0))
   (dolist (i l count)
     (setf count (1+ count)))))

(defun it-nth (n l)
  "An iterative version of nth. Returns the nth element of a list."
  (dotimes (i n (first l))
    (pop l)))

(defun it-union (a b)
  "An iterative version of union. Returns the union of a and b."
  (let ((result a))
    (dolist (element b result)
      (when (not (member element a))
        (push element result)))))

(defun check-all-odd (numbers)
  "Checks if all the numbers in a list are odd"
  (do((x numbers (rest x)))
     ((null x) t)
     (if (evenp (first x)) (return NIL))))

(defun find-largest (list-of-numbers)
  "Takes a list of numbers and returns the largest"
  (do*((x list-of-numbers (rest x))
       (e (first x) (first x))
       (largest e ))
       ((null x) largest)
       (when (> e  largest)
          (setf largest e))))

(defun nth-fib (n)
 "Returns the nth fibonacci number (starting with F0 = 0 and F1 = 1)"
  (do ((a 0 b)
        (b 1 (+ a b))
        (i n (1- i)))
       ((<= i 0) a )))

(defun power-of-2 (n)
  "2 to the nth power"
  (do ((result 1 (* 2 result))
       (i n (1- i)))
      ((<= i 0) result)))

(defun first-non-integer (x)
 "Return the first non-integer element of x or NONE"
 (dolist (i x 'none )
   (unless (integerp i)(return i))))
;;==============================================================
;;Keyboard exercise - DNA
(defun complement-base (base)
 "Returns the matching complementary base of a given base"
 (cond ((eql base 'A) 'T)
       ((eql base 'T) 'A)
       ((eql base 'G) 'C)
       ((eql base 'C) 'G)))

(defun complement-strand (strand)
  "Returns the complementary strand of a sequence of single-stranded DNA"
  (mapcar (lambda(x)(complement-base x)) strand))

(defun make-double (strand)
  "Takes a single strand of DNA and returns a double-stranded version"
  (do*((complements (complement-strand strand) (rest complements))
       (s strand (rest s))
       (double-strand (list (list(first s)(first complements)))  
         (cons (list (first s)(first complements)) double-strand)))
      ((= (length complements) 1)
           (reverse double-strand))))

(defun count-bases (strand)
 "Counts the number of bases of each type in DNA strand and returns the
  result as a table. This works for both single and double-stranded DNA"
  (do*((bases (flatten strand) (rest bases) )
       (next (first bases) (first bases))
       (a 0)
       (th 0)
       (g 0)
       (c 0))
       ((null bases) `((A ,a) (T ,th) (G ,g) (C ,c)))
       (cond ((eql next 'A) (setf a (1+ a)))
             ((eql next 'T) (setf th (1+ th)))
             ((eql next 'G) (setf g (1+ g)))
             ((eql next 'C) (setf c (1+ c))))))

(defun prefixp (strand-1 strand-2)
 "Returns true if DNA strand one is a prefix of strand two.
  Example: (G T C) is a prefix of (G T C A T), but not of (A G G T C)."
  (do* ((s1 strand-1 (rest s1))
        (s2 strand-2 (rest s2))
        (next-1 (first s1) (first s1))
        (next-2 (first s2) (first s2)))
        ((null s1) t)
        (unless (eql next-1 next-2) (return nil))))

(defun appearsp (strand-1 strand-2)
  "True if strand one appears anywhere in strand two"
  (cond((null strand-2) nil)
       ((prefixp strand-1 strand-2) t)
       ((appearsp strand-1 (rest strand-2)))))

(defun coverp (strand-1 strand-2)
  "T if its first input, repeated some number of times, matches all of 
  its second input. Example: (A G C) covers (A G C A G C A G C) but not 
   (A G C T T G)." 
  (cond((null strand-2) t)
       ((prefixp strand-1 strand-2) 
          (coverp strand-1 (nthcdr (length strand-1) strand-2)))
       (t nil)))

(defun prefix (n strand)
  "returns the leftmost N bases of a DNA strand. 
    (PREFIX 4 '(C G A T T A G)) should return (C G A T)."
 (do* ((s strand (rest s))
      (result (list (first s)) (cons (first s) result))
      (i n (1- i)))
      ((<= i 1) (reverse result))) )

(defun kernel (strand)
  "Returns the shortest prefix of a DNA strand that can be repeated to 
  cover the strand. (KERNEL '(A G C A G C A G C)) should return (A G C). 
  (KERNEL '(A A A A A)) should return (A). (KERNEL '(A G G T C)) should 
   return (A G G T C), because in this case only a single repetition of
    the entire strand will cover the strand."
  (do* ((s strand (rest s))
        (kernel (list (first s))(append kernel (list (first s)))))
        ((null s) kernel)
        (when (coverp kernel strand) (return kernel))))

(defun draw-n-times (n string)
 "Draws a string n times"
  (dotimes (i n) (format t string)))

(defun draw-dna (strand)
  "DRAW-DNA takes a single-stranded DNA
   sequence as input and draws it along with its complementary strand"
  (let((len (length strand)))
    (format t "~&")
    (draw-n-times len "-----")
    (format t "~&")
    (draw-n-times len "  !  ")
    (format t "~&")
   (dolist (i strand)
       (progn (format t "  ")
              (format t "~A" i)
              (format t "  ")))
     (format t "~&")
     (draw-n-times len "  .  ")
     (format t "~&")
     (draw-n-times len  "  .  ")
     (format t "~&")
   (dolist (i (complement-strand strand))
       (progn (format t "  ")
              (format t "~A" i)
              (format t "  ")))
     (format t "~&")
     (draw-n-times len "  !  ")
     (format t "~&")
     (draw-n-times len "-----")))
;;=======================================================================
(defun addup (n)
"Adds up the first N integers"
(do ((i 0 (+ i 1))
(sum 0 (+ sum i)))
((> i n) sum)))