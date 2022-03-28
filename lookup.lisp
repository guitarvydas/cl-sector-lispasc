
  
(defun lookup (name mem)
  (let ((prototypes (list *lookup* *scroll-through-atoms* *match-single-atom-name* *unsuccessful* *successful*)))
    (let ((top-context1 (instantiate *lookup* nil prototypes)))
      (mem-reset mem)
      (let ((top-context ($maybe-set-field top-context1 '$args `( (name . ,name) (atom-memory . ,mem) ))))
        ($dispatch top-context)))))


