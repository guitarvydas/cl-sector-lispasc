(defun check-lower-word (word)
    (loop
        for c across word 
        do (if (lower-case-p c) (return-from check-lower-word T))))

(defun testclw ()
  (check-lower-word "AbC"))