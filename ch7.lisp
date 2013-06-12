;;Common Lisp: A Gentle Introduction to Symbolic Computation 
;; Chapter 7
 (defparameter *note-table* 
   '((c       1)
     (c-sharp 2)
     (d       3)
     (d-sharp 4)
     (e       5)
     (f       6)
     (f-sharp 7)
     (g       8)
     (g-sharp 9)
     (a       10)
     (a-sharp 11)
     (b       12)))

(defun numbers (notes)
 "Takes a list of notes  and returns the corresponding list of numbers 
  based on *note-table*"
  (mapcar (lambda(x)(second (assoc x *note-table*))) notes))

(defun notes (numbers)
 "Takes a list of numbers as input and returns the corresponding list
  of notes"
     (let ((number-table (mapcar #'reverse *note-table*)))
       (mapcar (lambda (x) (second (assoc x number-table))) numbers)))

(defun raise (n numbers)
 "Takes n and a list of numbers and raises each number by n"
 (mapcar (lambda (x)(+ x n)) numbers))

(defun normalize (numbers)
 "Normalises numbers to make them between 1 and 12"
 (mapcar (lambda (x) (mod x 12)) numbers ))

(defun transpose (n song)
 "Takes n and a song as input and returns the song transposed by n half 
  steps"
  (let ((numbers (numbers song)))
       (notes (normalize (raise n numbers)))))

(defun all-odd (list)
 "True if every element of a list is odd"
 (every (lambda(x)(oddp x)) list))

(defun none-odd (list)
  "True if every element of a list of numbers is not odd"
 (every (lambda(x)(evenp x)) list))

(defun not-all-odd (list)
  "True if not every element of a list of numbers is odd"
  (not (all-odd list)))

(defun not-none-odd (list)
  "True if there is one or more odd elements"
  (not (none-odd list)))
;;Use of setf leads to undefined variable warning
(defparameter *database*
 '((b1 shape brick)
    (b1 color green)
    (b1 size small)
    (b1 supported-by b2)
    (b1 supported-by b3)
    (b2 shape brick)
    (b2 color red)
    (b2 size small)
    (b2 supports b1)
    (b2 left-of b3)
    (b3 shape brick)
    (b3 color red)
    (b3 size small)
    (b3 supports b1)
    (b3 right-of b2)
    (b4 shape pyramid)
    (b4 color blue)
    (b4 size large)
    (b4 supported-by b5)
    (b5 shape cube)
    (b5 color green)
    (b5 size large)
    (b5 supports b4)
    (b6 shape brick)
    (b6 color purple)
    (b6 size large)))

(defun match-element (a b)
 "True if symbols match or second symbol is ?"
 (or (eql a b)(eql b '?)))

(defun match-triple (assertion pattern)
 "True if the assertion matches the pattern"
 (every (lambda (x) (eql x T)) 
      (mapcar (lambda (x y) (match-element x y)) assertion pattern)))

(defun fetch (pattern)
  "Returns all assertions in the database that match the pattern"
  (remove-if-not (lambda(x) (match-triple x pattern)) *database*))

(defun supporters (block-name)
  "Takes a block name and returns list of blocks which support it"
  (mapcar (lambda(x)(third x))
      (fetch `(,block-name supported-by ?))))


(defun supp-cube (block-name) 
  "Takes a block as input and returns true if it is supported by a cube"
  (not(member NIL (mapcar (lambda (x) (fetch `(,x shape cube))) 
        (supporters block-name)))))

(defun desc1 (block-name)
 "Returns all assertions dealing with a block"
 (fetch `(,block-name ? ?)))

(defun desc2 (block-name)
  "Strips block name from result of desc1"
  (mapcar (lambda (x) (rest x)) (desc1 block-name)))

(defun description (block-name)
  "Returns the description of a block"
  (reduce #'append (desc2 block-name)))
