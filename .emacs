;;;; Paul's .emacs file
;; Originally written sometime before September 7, 2003
;; Time-stamp: <2013-07-04 14:19:54 pcurry>

;;; Note: This should be in the user's home directory (~).
;;; Or you could just make ~/.emacs a symbolic link to this file.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(global-font-lock-mode t nil (font-lock))
 '(initial-buffer-choice t)
 '(mouse-wheel-mode t nil (mwheel))
 '(save-place t nil (saveplace)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;; My lisp files directories
(add-to-list 'load-path "~/.emacs.d/")

;;; My user installed packages.
(add-to-list 'load-path "~/.emacs.d/packages")

;;; Autoloads of user installed packages.
(condition-case nil
    (load-file "~/.emacs.d/packages/loaddefs.el")
  (error (load-file "~/.emacs.d/packages/update-autoloads.el")
         (update-autoloads-for-packages)
         (load-file "~/.emacs.d/packages/loaddefs.el")))

;;; Load all my custom lisp files.
(mapc 'load-library
      '("functions"
        "variables"
        "modes"
        "templates"
        "keys"))
