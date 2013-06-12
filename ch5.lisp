(defun throw-die ()
 "Return a random number from 1 to 6 inclusive"
  (+ 1 (random 6)))

(defun throw-dice ()
  "Returns the values of two six-sided dice"
  (list (throw-die) (throw-die)))

(defun snake-eyesp (dice)
  "True if both dice are ones"
  (let ((die1 (first dice))
        (die2 (first (rest dice))))
    (and (eql die1 1)
         (eql die2 1))))

(defun box-carsp (dice)
  "True if both dice are sixes"
  (let ((die1 (first dice))
        (die2 (first (rest dice))))
    (and (eql die1 6)
         (eql die2 6))))


(defun instant-winp (dice) 
  "7 or 11 is instant win"
  (let* ((die1 (first dice))
        (die2 (second dice))
        (total (+ die1 die2)))
   (eql total (or 7 11) )))

(defun instant-lossp (dice)
  "2, 3 or 12 is instant loss"
  (let* ((die1 (first dice))
        (die2 (second dice))
        (total (+ die1 die2)))
    (eql total (or 2 3 12))))

(defun say-throw (dice)
  "Takes a dice throw as input and returns
   either the sum of the two dice or the symbol snake-eyes 
   or boxcars if the sum is 2 or 12"
   (cond ((snake-eyesp dice) 'snakeeyes)
         ((box-carsp dice) 'boxcars)
         (t (+ (first dice) (second dice)))))

(defun craps ()
  (let* ((dice (throw-dice))
         (die1 (first dice))
         (die2 (second dice)))
     `(throw ,die1 and ,die2 -- ,(say-throw dice) -- 
         ,(append 
            (cond ((instant-lossp dice) '(you lose))
                  ((instant-winp dice) '(you win))
                  (t `(your point is ,(say-throw dice))))))))