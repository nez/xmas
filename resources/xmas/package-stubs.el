;;; package-stubs.el --- Minimal package.el stubs for xmas

(defvar package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
(defvar package-archive-contents nil)
(defvar package-user-dir (expand-file-name "~/.xmas/elpa"))
(defvar package--initialized nil)

(defun package-initialize (&optional no-activate)
  (setq package--initialized t))

(defun package-installed-p (pkg &optional min-version)
  (featurep pkg))

(defun package-install (pkg &optional dont-select)
  (message "Package install not supported: %s (place .el files in ~/.xmas/lisp/)" pkg))

(defun package-refresh-contents (&optional async)
  (message "Package refresh not supported (place .el files in ~/.xmas/lisp/)"))

(defun package-activate-all () nil)

(provide 'package)
