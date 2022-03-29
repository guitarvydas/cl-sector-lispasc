;; JavaScript-like variables with string names
;; each object has an 'own' context
;; the 'own' context contains a list of own variables
;; the 'own' context contains a pointer to the parent's context, the pointer is called 'container'
;; the parent/child relationship is dynamic (not static, like in classes)

;; see context.drawio (diagram)

;; lookup: try to find the given string in the 'own' portion of an object's context
;;  if not found, try lookup again in ancestor's context, recursively
;;   if not found in any context, return (values nil nil) (value=nil, success=nil)
;;   if     found                 return (values v   t)   (value=v,   success=t)
;; to set a named variable to v:
;;   if the named slot does not exist in 'own', create it and set its value to v
;;   if the named slot does     exist in 'own', overwrite the slot with v
;;

;; in this experimental implementation, a context is an alist of (field-symbol . <ht>) pairs
;;   and a field is a hash table capable of hashing strings

(defun ancestor-context-of (context)
  (let ((ancestor-context (assoc 'container context)))
    (if ancestor-context
        (car ancestor-context)
      nil)))

(defun lookup-shallow (context string)
  (let ((table (cdr (assoc 'own context))))
    (gethash string table)))

(defun lookup-deep (context string)
  (if (null context)
      (values nil nil)
    (multiple-value-bind (val success)
        (lookup-shallow context string)
      (if success
          (values val t)
        (lookup-shallow (ancestor-context-of context) string)))))

(defun set-shallow (context string val)
  (let ((table (cdr (assoc 'own context))))
    (setf (gethash string table)
          val))
  (values val t))

(defun augment-context-with-field (context field-symbol initial-value)
  (cons (cons field-symbol initial-value) context))

(defun make-slots ()
  (make-hash-table :test 'equal))

(defun make-context ()
  `( (container . nil) (own . ,(make-slots))))

;;; tests

(defun test0 ()
  (let ((context `( (container . nil) (own . ,(make-slots)) )))
    (set-shallow context "hello" "world")
    (lookup-shallow context "hello")))

(defun test1 ()
  (let ((context (make-context)))
    (set-shallow context "hello" "world")
    (lookup-shallow context "hello")))

(defun test2 ()
  (let ((context-parent (make-context))) (declare (ignore context-parent))
    (let ((context (make-context)))
      (set-shallow context "hello" "world 2")
      (lookup-shallow context "hello"))))

(defun test3 ()
  (let ((context-parent (make-context)))
    (let ((context (augment-context-with-field (make-context) 'container context-parent)))
      (set-shallow context "hello" "world 3")
      (format *standard-output* "child context = ~a~%" context)
      (lookup-shallow context "hello"))))

(defun test4 ()
  (let ((context-parent (make-context)))
    (let ((context (augment-context-with-field (make-context) 'container context-parent)))
      (set-shallow context "hello" "world 4")
      (format *standard-output* "child context = ~a~%" context)
      (lookup-deep context "hello"))))

#+nil(defun test5 ()
  (let ((context-parent (make-context)))
    (let ((context (augment-context-with-field (make-context) 'container context-parent)))
      (set-shallow context-parent "hello" "parent world 4")
      (format *standard-output* "child context = ~a~%" context)
      (lookup-deep context "hello"))))

(defun test ()
  (test4))
