;;; subr.el --- basic Emacs Lisp subroutines for xmas

;; --- Conditional macros ---

(defmacro when (cond &rest body)
  `(if ,cond (progn ,@body)))

(defmacro unless (cond &rest body)
  `(if ,cond nil (progn ,@body)))

;; --- Iteration macros ---

(defmacro dolist (spec &rest body)
  (let ((var (car spec))
        (list-form (car (cdr spec))))
    `(let ((--dolist-tail-- ,list-form))
       (while --dolist-tail--
         (let ((,var (car --dolist-tail--)))
           ,@body)
         (setq --dolist-tail-- (cdr --dolist-tail--))))))

(defmacro dotimes (spec &rest body)
  (let ((var (car spec))
        (count-form (car (cdr spec))))
    `(let ((--dotimes-limit-- ,count-form)
           (,var 0))
       (while (< ,var --dotimes-limit--)
         ,@body
         (setq ,var (1+ ,var))))))

;; --- Progn variants ---

(defmacro prog1 (first &rest body)
  `(let ((--prog1-val-- ,first))
     ,@body
     --prog1-val--))

(defmacro prog2 (first second &rest body)
  `(progn
     ,first
     (let ((--prog2-val-- ,second))
       ,@body
       --prog2-val--)))

;; --- Stack macros ---

(defmacro push (val place)
  `(setq ,place (cons ,val ,place)))

(defmacro pop (place)
  `(prog1 (car ,place)
     (setq ,place (cdr ,place))))

;; --- List accessors ---

(defun cadr (x) (car (cdr x)))
(defun caddr (x) (car (cdr (cdr x))))
(defun caar (x) (car (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun cdar (x) (cdr (car x)))

;; --- List search ---

(defun member (elt list)
  (let ((result nil))
    (while (and list (not result))
      (if (equal elt (car list))
        (setq result list)
        (setq list (cdr list))))
    result))

(defun assoc (key alist)
  (let ((result nil))
    (while (and alist (not result))
      (if (and (consp (car alist)) (equal key (car (car alist))))
        (setq result (car alist))
        (setq alist (cdr alist))))
    result))

(defun assq (key alist)
  (let ((result nil))
    (while (and alist (not result))
      (if (and (consp (car alist)) (equal key (car (car alist))))
        (setq result (car alist))
        (setq alist (cdr alist))))
    result))

;; --- Utility ---

(defun nthcdr (n list)
  (let ((i 0))
    (while (< i n)
      (setq list (cdr list))
      (setq i (1+ i)))
    list))

(provide 'subr)
