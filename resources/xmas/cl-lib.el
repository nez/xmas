;;; cl-lib.el --- Common Lisp extensions for xmas

;; --- Aliases ---

(defmacro cl-defun (name args &rest body)
  (cons 'defun (cons name (cons args body))))

;; --- cl-case ---

(defmacro cl-case (expr &rest clauses)
  (let ((var (intern "--cl-case-val--")))
    (let ((cond-clauses
           (mapcar (lambda (clause)
                     (let ((key (car clause))
                           (body (cdr clause)))
                       (if (equal key 't)
                         (cons 't body)
                         (if (listp key)
                           (cons (list 'member var (list 'quote key)) body)
                           (cons (list 'equal var (list 'quote key)) body)))))
                   clauses)))
      (list 'let (list (list var expr))
            (cons 'cond cond-clauses)))))

;; --- Filter / search ---

(defun cl-remove-if (pred list)
  (let ((result nil))
    (dolist (x list)
      (unless (funcall pred x)
        (push x result)))
    (reverse result)))

(defun cl-remove-if-not (pred list)
  (cl-remove-if (lambda (x) (not (funcall pred x))) list))

(defun cl-find-if (pred list)
  (let ((result nil))
    (while (and list (not result))
      (when (funcall pred (car list))
        (setq result (car list)))
      (setq list (cdr list)))
    result))

(defun cl-position (elt list)
  (let ((idx 0) (result nil))
    (while (and list (not result))
      (if (equal elt (car list))
        (setq result idx)
        (progn (setq idx (1+ idx))
               (setq list (cdr list)))))
    result))

(defun cl-reduce (fn list &optional initial-value)
  (let ((acc (if initial-value initial-value (car list)))
        (rest-list (if initial-value list (cdr list))))
    (while rest-list
      (setq acc (funcall fn acc (car rest-list)))
      (setq rest-list (cdr rest-list)))
    acc))

(defun cl-mapcar (fn &rest lists)
  (mapcar fn (car lists)))

;; --- Increment / Decrement ---

(defmacro cl-incf (place &optional delta)
  (list 'setq place (list '+ place (or delta 1))))

(defmacro cl-decf (place &optional delta)
  (list 'setq place (list '- place (or delta 1))))

;; --- cl-block / cl-return-from ---

(defmacro cl-block (name &rest body)
  (list 'condition-case '--cl-block-val--
    (cons 'progn body)
    (list 'cl-block-exit
      (list 'if (list 'equal (list 'car (list 'cdr '--cl-block-val--)) (list 'quote name))
        (list 'car (list 'cdr (list 'cdr '--cl-block-val--)))
        (list 'signal (list 'quote 'cl-block-exit) (list 'cdr '--cl-block-val--))))))

(defmacro cl-return-from (name &optional value)
  (list 'signal (list 'quote 'cl-block-exit) (list 'list (list 'quote name) value)))

;; --- cl-destructuring-bind ---

(defun cl--destructure-bindings (pattern val-sym)
  (let ((bindings nil) (idx 0))
    (while pattern
      (let ((var (car pattern)))
        (if (equal var (intern "&rest"))
          (progn
            (push (list (car (cdr pattern)) (list 'nthcdr idx val-sym)) bindings)
            (setq pattern nil))
          (progn
            (push (list var (list 'nth idx val-sym)) bindings)
            (setq idx (1+ idx))
            (setq pattern (cdr pattern))))))
    (reverse bindings)))

(defmacro cl-destructuring-bind (pattern expr &rest body)
  (let ((val (intern "--cl-db-val--")))
    (let ((bindings (cl--destructure-bindings pattern val)))
      (list 'let (list (list val expr))
            (cons 'let (cons bindings body))))))

;; --- cl-labels ---

(defmacro cl-labels (fndefs &rest body)
  (let ((bindings (mapcar (lambda (fndef)
                            (list (car fndef)
                                  (cons 'lambda (cdr fndef))))
                          fndefs)))
    (cons 'let (cons bindings body))))

;; --- cl-loop (simplified) ---

(defmacro cl-loop (&rest body)
  (cond
    ;; for VAR in LIST collect EXPR
    ((and (equal (car body) 'for)
          (equal (nth 2 body) 'in)
          (equal (nth 4 body) 'collect))
     (let ((var (nth 1 body))
           (list-form (nth 3 body))
           (collect-form (nth 5 body)))
       (list 'let (list (list '--result-- nil))
         (list 'dolist (list var list-form)
           (list 'push collect-form '--result--))
         (list 'reverse '--result--))))
    ;; for VAR from N to M collect EXPR
    ((and (equal (car body) 'for)
          (equal (nth 2 body) 'from)
          (equal (nth 4 body) 'to)
          (equal (nth 6 body) 'collect))
     (let ((var (nth 1 body))
           (start (nth 3 body))
           (end (nth 5 body))
           (collect-form (nth 7 body)))
       (list 'let (list (list '--result-- nil) (list var start))
         (list 'while (list '<= var end)
           (list 'push collect-form '--result--)
           (list 'setq var (list '1+ var)))
         (list 'reverse '--result--))))
    (t (error "Unsupported cl-loop form"))))

(provide 'cl-lib)
