(defun add-up(numbers)
  "Recursively adds up numbers in list"
  (if (null numbers) 0
      (+ (first numbers) (add-up (rest numbers)))))

(defun alloddp (numbers)
  "True if all numbers in a list are odd"
  (cond ((null numbers) T)
        ((evenp (first numbers)) '())
        (T (alloddp (rest numbers)))))

(defun fib(n)
  "Recursive fibonacci"
  (cond ((= n 1)1)
        ((= n 0)1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

(defun find-first-odd (numbers)
 "Returns the first odd number in a list or nil"
 (cond ((null numbers)'())
       ((oddp (first numbers)) (first numbers))
       (t (find-first-odd (rest numbers)))))

(defun count-atoms(tree)
  (cond ((null tree)1)
        ((atom tree)1)
        (t(+ (count-atoms (first tree))
              (count-atoms (rest tree)))) ))

(defun sum-tree (tree)
  "The sum of all numbers in a tree. Non-numbers are ignored."
  (cond ((null tree) 0)
        ((numberp tree) tree)
        ((and (atom tree) (not (numberp tree))) 0)
        (t (+ (sum-tree (car tree))
              (sum-tree (cdr tree))))))

(defun flatten (tree)
  "Returns all the elements of a nested list in a single-level list"
  (cond ((null tree) '())
        ((atom tree) (list tree))
        (t (append (flatten (first tree))
                   (flatten (rest tree))))))

(defun every-other (list)
 "Returns every other element of a list"
  (cond ((null list)'())
        (t (cons (first list) (every-other (rest (rest list)))))))

(defun left-half (l)
 "Returns the first n/2 elements of a list of length n"
  (labels ((aux (l halfway)
    (if (= (length l) halfway) '()
        (cons (first l) (aux (rest l) halfway)))))
     (aux l (ceiling (/ (length l) 2)))) )

(defun merge-lists (l1 l2)
 "Merges two sorted lists"
 (cond ((null l1) l2)
       ((null l2) l1)
       ((<= (first l1) (first l2))
          (cons (first l1) (merge-lists (rest l1) l2)))
       (t (cons (first l2) (merge-lists l1 (rest l2))))))

;;Geneology exercise

(defvar  *family*
 '((colin nil nil)
    (deirdre nil nil)
    (arthur nil nil)
    (kate nil nil)
    (frank nil nil)
    (linda nil nil)
    (suzanne colin deirdre)
    (bruce arthur kate)
    (charles arthur kate)
    (david arthur kate)
    (ellen arthur kate)
    (george frank linda)
    (hillary frank linda)
    (andre nil nil)
    (tamara bruce suzanne)
    (vincent bruce suzanne)
    (wanda nil nil)
    (ivan george ellen)
    (julie george ellen)
    (marie george ellen)
    (nigel andre hillary)
    (frederick nil tamara)
    (zelda vincent wanda)
    (joshua ivan wanda)
    (quentin nil nil)
    (robert quentin julie)
    (olivia nigel marie)
    (peter nigel marie)
    (erica nil nil)
    (yvette robert zelda)
    (diane peter erica)))

(defun father (person)
 "Returns the father of the person"
  (second (Assoc person *family*)))

(defun mother (person)
  "Returns the mother of the person"
  (third (assoc person *family*)))

(defun parents (person)
  "Returns the parents of a person"
  (remove-if #'null (rest (assoc person *family*))))

(defun children (person)
  "Returns the children of the person"
  (mapcar (lambda(x)(first x)) 
    (union 
    (remove-if-not (lambda(x)(eql (second x) person))*family*)
    (remove-if-not (lambda(x)(eql (third  x) person))*family*))))

(defun siblings (person)
  (let((parents (parents person)))
    (remove-if (lambda (x)(eql x person))(union (children (first parents))
           (children (second parents))))))

(defun mapunion (fn l)
  "Applies a function to every element of a list and returns the union 
   of the results"
  (reduce #'union (mapcar fn l)))

(defun grandparents (person)
  "Returns the grandparents of a person"
  (let((parents (parents person)))
    (mapunion #'parents parents)))

(defun cousins (person)
 "Returns the cousins of a person;the children of their parents' siblings"
  (let* ((parents (parents person))
         (parents-siblings (mapunion #'siblings parents))
         (cousins (mapunion #'children parents-siblings)))
         cousins))

(defun descended-from (person1 person2)
  "True if the first person is descended from the second."
  (let((parents (parents person1)))
    (cond ((null parents) NIL)
       ((member person2 parents) t)
       (t(or(descended-from (mother person1) person2)
          (descended-from (father person1) person2))))))

(defun ancestors (person)
 "Returns a person's set of ancestors"
  (let((parents (parents person)))
    (if (null parents) '()
        (append parents
               (mapunion #'ancestors parents)))))

(defun generation-gap (person ancestor)
 "Returns the number of generations separating someone from an ancestor"
  (labels ((aux (person gap)
    (let ((parents (parents person)))
       (cond ((null parents) NIL)
             ((member ancestor parents) gap)
             (t (or (aux (mother person) (1+ gap))
                    (aux (father person) (1+ gap)))))) ))
     (aux person 1)))

(defun tree-find-if (fn tree)
  "Returns the first non-nil atom of a tree which satisfies a predicate"
  (cond ((null tree) NIL)
        ((atom tree) (if (funcall fn tree) tree NIL))
        (t (or (tree-find-if fn (first tree))
               (tree-find-if fn (rest tree))))))