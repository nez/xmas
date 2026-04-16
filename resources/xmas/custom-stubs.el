;;; custom-stubs.el --- Minimal custom.el stubs for xmas

(defvar custom--faces nil "Stored face settings from custom-set-faces.")

(defmacro defgroup (name _members docstring &rest _args)
  `(progn (put ',name 'group-documentation ,docstring) ',name))

(defmacro defface (face spec docstring &rest _args)
  `(progn (defvar ,face ',spec ,docstring) ',face))

(defun custom-set-variables (&rest args)
  "Apply customized variable settings."
  (while args
    (let ((entry (car args)))
      (when (and (listp entry) (car entry))
        (condition-case nil
          (set (car entry) (eval (car (cdr entry))))
          (error nil)))
      (setq args (cdr args)))))

(defun custom-set-faces (&rest args)
  "Store face settings for later application."
  (setq custom--faces args))

(defun custom-initialize-default (sym exp)
  (unless (default-boundp sym)
    (set-default sym (eval exp))))

(defun custom-declare-variable (sym val docstring &rest _args)
  (unless (boundp sym)
    (set sym (eval val)))
  (when docstring (put sym 'variable-documentation docstring))
  sym)

(provide 'custom)
(provide 'cus-face)
(provide 'wid-edit)
(provide 'widget)
