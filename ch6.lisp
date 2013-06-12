(defun last-element (l)
  "Returns the last element of a list"
  (car (last l)))

(defun next-to-last (l)
  "Returns the next to last element of a list"
  (second (reverse l)))

(defun my-butlast (l)
  "Returns a list with the last element removed"
  (reverse (cdr (reverse l))))

(defun palindromep (l)
  "Returns T if the list is a palindrome"
  (equal (reverse l) l))

(defun make-palindrome (l)
  "Makes a palindrome out of a list"
  (append l (reverse l)))

(defun set-equal (a b)
 "True if set a and set b are equal"
 (and (subsetp a b)(subsetp b a)))

(defun proper-subsetp (a b)
 "True if a is a proper subset (a subset but not equal) of b"
 (and (subsetp a b)(not (subsetp b a))))

;;Page 178 Mini keyboard exercise
(defparameter *feature-list* 
     '(large red shiny cube -vs- small shiny red four-sided pyramid))

(defparameter  *feature-list2* 
     '(small red metal cube -vs- red plastic small cube))

(defun right-side (l)
  "Returns everything to the right side of -VS-"
  (cdr (member '-VS- l)))

(defun left-side (l)
  "Returns everything to the left side of -VS-"
  (let((symbol (first l)))
     (if (eql symbol '-VS-) '() 
          (cons symbol (left-side (rest l)))) ))

(defun count-common (features)
  "Returns the number of features that the left and right sides have in
   common"
  (let ((left (left-side features))
        (right (right-side features)))
       (length (intersection left right))))

(defun compare (features)
  "Takes a list of features with a -VS- between and reports the number
   of features that they have in common"
  `(,(count-common features) common features))

;;Page 197 Review Exercises
(defun swap-first-last (l)
  "Returns list with first and last elements swapped"
  (let*((e1 (first l))
        (e2 (first (reverse l)))
        (without-e1 (rest l))
        (middle (reverse (rest (reverse without-e1)))))
      (cons e2 (append middle (list e1)))))

(defun rotate-left (l)
  "Rotate the elements of a list left"
  (append (member (second l) l) (list (first l))))

(defun rotate-right (l)
  "Rotate the elements of a list right"
  (append (last l) (butlast l)))

;;Page 188 Keyboard exercise
(defparameter *rooms*
'((living-room (north front-stairs)
(south dining-room)
(east kitchen))
(upstairs-bedroom (west library)
(south front-stairs))
(dining-room (north living-room)
(east pantry)
(west downstairs-bedroom))
(kitchen (west living-room)
(south pantry))
(pantry (north kitchen)
(west dining-room))
(downstairs-bedroom (north back-stairs)
(east dining-room))
(back-stairs (south downstairs-bedroom)
(north library))
(front-stairs (north upstairs-bedroom)
(south living-room))
(library (east upstairs-bedroom)
(south back-stairs))))

(defparameter *loc* 'pantry)

(defun choices (room)
  "Takes the name of a room and returns a table of permissable directions
   from *rooms*"
  (rest (Assoc room *rooms*)))

(defun look (direction room)
 "Says where Robbie would end up if he moved in that direction from a room"
  (last-element (assoc direction (choices room))))

(defun set-robbie-location (place)
   "Moves Robbie to PLACE by setting the variable LOC."
   (setf *loc* place))

(defun how-many-choices ()
 "Returns the number of choices Robbie has for where to move next from 
  his current location *loc*"
  (length (choices *loc*)))

(defun upstairsp (room)
  "True if the room is upstairs"
  (or (eql room 'upstairs-bedroom)
      (eql room 'library)))

(defun onstairsp (room)
  "True if front stairs or back stairs"
  (or (eql room 'front-stairs)
      (eql room 'back-stairs)))

(defun where ()
 "Where's Robbie?"
  (cond ((onstairsp *loc*)
         `(robbie is on the ,*loc*))
        ((upstairsp *loc*)
         `(robbie is upstairs in the ,*loc*))
        (t `(robbie is downstairs in the ,*loc*))))

(defun move (direction)
  "Move Robbie in a direction"
 (let ((other-room (look direction *loc*)))
  (if other-room
      (progn (set-robbie-location (look direction *loc*))
             (where))
      '(Ouch! Robbie hit a wall))))