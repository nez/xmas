;;; use-package.el --- Simplified use-package for xmas

(defun use-package--parse-args (args)
  "Parse use-package keyword args into alist of (keyword . body-forms)."
  (let ((sections nil) (key nil) (body nil))
    (dolist (item args)
      (if (and (symbolp item) (not (equal item t)) (not (equal item nil))
               (string-prefix-p ":" (symbol-name item)))
        (progn (when key (push (cons key (reverse body)) sections))
               (setq key item body nil))
        (push item body)))
    (when key (push (cons key (reverse body)) sections))
    (reverse sections)))

(defun use-package--get (key sections)
  "Get the body forms for KEY from parsed sections."
  (cdr (assoc key sections)))

(defmacro use-package (name &rest args)
  "Simplified use-package: handles :ensure :defer :init :config :bind :hook :custom :commands."
  (let ((sections (use-package--parse-args args)))
    (let ((init-body   (use-package--get :init sections))
          (config-body (use-package--get :config sections))
          (bind-list   (car (use-package--get :bind sections)))
          (hook-list   (car (use-package--get :hook sections)))
          (custom-list (car (use-package--get :custom sections)))
          (cmd-list    (use-package--get :commands sections))
          (has-defer   (assoc :defer sections)))
      ;; Build the expansion
      (let ((forms nil))
        ;; :custom -> setq
        (dolist (pair (or custom-list nil))
          (push (list 'setq (car pair) (car (cdr pair))) forms))
        ;; :init body
        (dolist (f init-body) (push f forms))
        ;; require or autoload
        (if has-defer
          (dolist (cmd (or cmd-list (list name)))
            (push (list 'autoload (list 'quote cmd)
                        (symbol-name name) nil t) forms))
          (push (list 'condition-case nil
                      (list 'require (list 'quote name))
                      (list 'error nil)) forms))
        ;; :bind
        (dolist (b (or bind-list nil))
          (push (list 'global-set-key (list 'kbd (car b))
                      (list 'quote (cdr b))) forms))
        ;; :hook
        (dolist (h (or hook-list nil))
          (push (list 'add-hook
                      (list 'quote (intern (concat (symbol-name (car h)) "-hook")))
                      (list 'quote (cdr h))) forms))
        ;; :config body
        (dolist (f config-body) (push f forms))
        (cons 'progn (reverse forms))))))

(provide 'use-package)
