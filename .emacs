;;;; Paul's .emacs file
;; Originally written sometime before September 7, 2003
;; Time-stamp: <2013-07-01 11:11:24 pcurry>

;;; Note: This should be in the user's home directory (~).

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

;;;; My lisp files directories
(add-to-list 'load-path "~/.elisp/")

;;;; Let's just try out the default install for now
;; (add-to-list 'load-path "~/.elisp/site-lisp")
;; (add-to-list 'load-path "~/.elisp/site-lisp/nxml-mode")

(mapc 'load-library
      '("functions"
        "variables"
        "modes"
        "templates"
        "keys"))
