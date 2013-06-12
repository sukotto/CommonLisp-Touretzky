(defmacro simple-incf (var &optional (amount 1))
  (list 'setq var (list '+ var amount)))

(defmacro set-nil (var)
  "A macro that sets a variable to NIL"
  `(setq ,var NIL))

(defmacro simple-rotatef (a b)
  "Switches the value of two variables"
  `(let((a-val ,a)
        (b-val ,b))
     (progn (setq ,b a-val)
            (setq ,a b-val))))


(defmacro set-mutual (a b)
  "Takes two variable names as input and sets each variable to 
   the name of the other."
   `(let ((a-name ',a)
          (b-name ',b))
      (progn (setq ,b a-name)
             (setq ,a b-name))))

(defmacro showvar (var)
  `(format t "~&The value of ~S is ~S" ',var ,var) )

(defmacro variable-chain (&rest variables)
  "A macro called VARIABLE-CHAIN that accepts any number of
inputs. The expression (VARIABLE-CHAIN A B C D) should expand
into an expression that sets A to 'B, B to 'C, and C to 'D."
    `(progn ,@(mapcar (lambda (x y)
         `(setq ,x ',y) ) variables (rest variables))))