
  
(defun lookup (name mem)
  (let ((prototypes (list *lookup* *scroll-through-atoms* *match-single-atom-name* *unsuccessful* *successful*)))
    (let ((top-context1 (instantiate *lookup* nil prototypes)))
      (mem-reset mem)
      (let ((top-context ($maybe-set-field top-context1 '$args `( (junk . nil) (name . ,name) (atom-memory . ,mem) ))))
	($!local top-context 'name name)
	($!local top-context 'atom-memory mem)
        ($!local top-context 'name-to-be-matched name)
	($!local top-context 'found nil)
	($!local top-context 'answer nil)
	($inject '("scroll through atoms" . "name") name top-context nil)
        ($dispatch top-context)
        (values 
         ($?local top-context 'answer)
         ($?local top-context 'found))))))


