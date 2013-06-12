;; Array exercise - histograms
(defparameter  *hist-array* nil)
(defparameter  *total-points* nil)
(defun new-histogram (number-of-bins)
  (progn (setf *total-points* 0)
         (setf *hist-array* (make-array number-of-bins))))

(defun record-value (n)
  (if (and (>= n 0) (< n (length *hist-array*)))
      (progn (incf (aref *hist-array* n) 1)
             (incf *total-points* 1))
      (error "record-value input is out of range")))

(defun print-hist-line (i)
  (format t "~2S" i )
  (format t "[~3S]" (aref *hist-array* i))
  (dotimes (var (aref *hist-array* i)) (format t "*"))
  (format t "~%"))

(defun print-histogram ()
  (dotimes (i (length *hist-array*)) (print-hist-line i)))

;;Hash table exercise - cryptograms
(defparameter *crypto-text* nil)
(defparameter *encipher-table* nil)
(defparameter *decipher-table* nil)
(setf *encipher-table* (make-hash-table))
(setf *decipher-table* (make-hash-table))

(defun make-substitution (a b)
  (progn
  (setf (gethash a *decipher-table*) b)
  (setf (gethash b *encipher-table*) a)))

(defun undo-substitution (letter)
  (let ((deciphered (gethash letter *decipher-table*)))
       (setf (gethash letter *decipher-table*) nil)
       (setf (gethash deciphered *encipher-table*) nil)))

(defun clear()
 (clrhash *decipher-table*)
 (clrhash *encipher-table*))

(defun decipher-string (encoded)
  "Takes a single encoded string as input and returns a new,
   partially decoded string."
  (let*((len (length encoded))
        (decoded (make-string len :initial-element #\Space)))
     (dotimes (i len decoded)
       (let((letter (gethash (aref encoded i) *decipher-table*)))
            (when letter (setf (aref decoded i) letter))))))

(defun show-line (cryptogram-line)
  "Displays one line of cryptogram text with the deciphered text
   beneath it."
  (format t "~S~%~S" cryptogram-line (decipher-string cryptogram-line)))

(defun show-text (cryptogram)
  "Takes a cryptogram (list of strings) as input and displays the lines"
  (dolist (line cryptogram)
    (show-line line)
    (format t "~%~%")))

(defun get-first-char (x)
  (char-downcase 
     (char (format nil "~A" x) 0)))

(defun read-letter()
  (let((input (read)))
    (if (or (equal input 'end)
            (equal input 'undo)) 
             input
         (get-first-char input))))

(defun sub-letter (ch)
 (let ((already-deciphered (gethash ch *decipher-table*)))
  (cond
       (already-deciphered
           (error (format t "~%But ~S already deciphers to ~S!"
               ch already-deciphered)))
       (t (format t "What does ~S decipher to?" ch)
          (let ((letter (read-letter)))
             (cond ((not (characterp letter))
                       (error (format t "~%~S is not a letter!" letter)))
               (t (let ((enciphered (gethash letter *encipher-table*)))
                 (if enciphered 
                   (error (format t "~%~S already enciphers to ~S" 
                     letter enciphered))
                   (make-substitution ch letter))))))))))

(defun undo-letter()
  (format t "Undo which letter?")
  (let* ((letter (read-letter))
         (deciphered (gethash letter *decipher-table*)))
         (if deciphered 
                (undo-substitution letter)
          (error (format t "The letter ~S has not been deciphered yet!" 
              letter)))))

(defun solve (cryptogram)
  (do ((letter nil))
      ((equal letter 'end))
      (show-text cryptogram)
      (format t "Substitute which letter?")
       (setf letter (read-letter))
       (cond ((characterp letter) (sub-letter letter))
             ((equal 'undo letter) (undo-letter))
             ((equal 'end letter) t))))

(defparameter *message* 
  '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
    "enlpo pib slafml pvv bfwkj"))
