;; JavaScript-like variables with string names
;; each object has an 'own' context
;; the 'own' context contains a list of own variables (currently called 'locals')
;; the 'own' context contains a pointer to the parent's context, the pointer is called 'container'
;; the parent/child relationship is dynamic (not static, like in classes)

;; lookup: try to find the given string in the 'locals' portion of an object's 'own' context
;;  if not found, try lookup again in ancestor's context, recursively
;;   if not found in any context, return (values nil nil) (value=nil, success=nil)
;;   if     found                 return (values v   t)   (value=v,   success=t)
;; to set a named variable to v:
;;   if the named slot does not exist in 'own', create it and set its value to v
;;   if the named slot does     exist in 'own', overwrite the slot with v
;;
;; todo: rename 'locals' to 'own'

;; in this experimental implementation, a context is an alist of (field-symbol . <ht>) pairs
;;   and a field is a hash table capable of hashing strings

(defun ancestor-context-of (context)
  (let ((ancestor-context (assoc context 'container)))
    (if ancestor-context
        (car ancestor-context)
      nil)))

(defun lookup-shallow (context field-symbol string)
  (let ((table (cdr (assoc field-symbol context))))
    (gethash string table)))

(defun lookup-deep (context field-symbol string)
  (if (null context)
      (values nil nil)
    (multiple-value-bind (val success)
        (lookup-shallow context field-symbol string)
      (if success
          (values val t)
        (lookup-shallow (ancestor-context-of context) field-symbol string)))))

(defun set-shallow (context field-symbol string val)
  (let ((table (cdr (assoc field-symbol context))))
    (setf (gethash string table)
          val))
  (values val t))

(defun augment-context-with-field (context field-symbol)
  ;; return context or new context
  (let ((success (assoc field-symbol context)))
    (if success
        context
      (cons (cons field-symbol (make-hash-table :test 'equal)) context))))
                  

(defun test0 ()
  (let ((context `( (own . ,(make-hash-table :test 'equal)) )))
    (set-shallow context 'own "hello" "world")
    (lookup-shallow context 'own "hello")))

(defun test1 ()
  (let ((context (augment-context-with-field nil 'own)))
    (set-shallow context 'own "hello" "world")
    (lookup-shallow context 'own "hello")))

(defun test ()
  (test1))