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
  (assoc key alist))

;; --- Utility ---

(defun nthcdr (n list)
  (dotimes (_ n)
    (setq list (cdr list)))
  list)

;; error handling

(defmacro ignore-errors (&rest body)
  `(condition-case nil (progn ,@body) (error nil)))

;; buffer-local convenience

(defmacro setq-local (sym val)
  `(progn (make-local-variable ',sym) (setq ,sym ,val)))

;; minor mode infrastructure

(defmacro define-minor-mode (name docstring &rest args)
  (let ((hook-name (intern (concat (symbol-name name) "-hook")))
        (map-name  (intern (concat (symbol-name name) "-map")))
        (keymap-expr nil)
        (body args))
    ;; Parse :keymap keyword arg
    (when (and args (equal (car args) :keymap))
      (setq keymap-expr (car (cdr args)))
      (setq body (cdr (cdr args))))
    `(progn
       (defvar ,name nil ,docstring)
       (defvar ,hook-name nil)
       (defvar ,map-name ,(or keymap-expr '(make-sparse-keymap)))
       (defun ,name (&optional arg)
         (interactive)
         ,docstring
         (setq ,name (if arg (> arg 0) (not ,name)))
         (if ,name
           (progn (use-local-map ,map-name) ,@body)
           (use-local-map nil))
         (run-hooks ',hook-name)
         ,name))))

;; --- buffer macros ---

(defmacro with-current-buffer (buf &rest body)
  `(save-current-buffer (set-buffer ,buf) ,@body))

(defmacro with-temp-buffer (&rest body)
  (let ((buf (intern "--with-temp-buffer--")))
    `(let ((,buf (get-buffer-create " *temp*")))
       (save-current-buffer (set-buffer ,buf) (erase-buffer) ,@body))))

;; --- string utilities ---

(defun string-empty-p (s) (or (null s) (equal s "")))

(defun string-join (strings &optional separator)
  (mapconcat 'identity strings (or separator "")))

;; --- list utilities ---

(defun ensure-list (obj)
  (if (listp obj) obj (list obj)))

(defun alist-get (key alist &optional default)
  (let ((pair (assoc key alist)))
    (if pair (cdr pair) default)))

;; --- define-derived-mode (simplified) ---

(defmacro define-derived-mode (child parent name &optional docstring &rest body)
  (let ((hook-name (intern (concat (symbol-name child) "-hook")))
        (map-name  (intern (concat (symbol-name child) "-map"))))
    `(progn
       (defvar ,hook-name nil)
       (defvar ,map-name (make-sparse-keymap))
       ,(when parent `(set-keymap-parent ,map-name
            (condition-case nil (symbol-value (intern (concat (symbol-name ',parent) "-map")))
              (error nil))))
       (defun ,child ()
         ,docstring
         (interactive)
         (setq major-mode ',child)
         (setq mode-name ,name)
         (use-local-map ,map-name)
         ,@body
         (run-hooks ',hook-name)))))

(provide 'subr)
