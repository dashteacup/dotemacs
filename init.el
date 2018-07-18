;;;; Paul's init.el file
;; Originally written sometime before September 7, 2003
;; Time-stamp: <2018-07-18 12:35:03 pcurry>

;;; This used to be my .emacs file, but Emacs 22 and up allows you to
;;; use ~/.emacs.d/init.el as your config file, so it got a name change.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ido-vertical-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; These paths are constants.
(let* ((elisp-dir "~/.emacs.d/my-lisp/")
       (package-dir "~/.emacs.d/packages/")
       (loaddefs (concat package-dir "loaddefs.el"))
       (autoloader (concat package-dir "update-autoloads.el")))

  ;; My lisp files directories.
  (add-to-list 'load-path elisp-dir)

  ;; My user installed packages.
  (add-to-list 'load-path package-dir)

  ;; Autoloads of user installed packages.
  (unless (file-exists-p loaddefs)
    (load-file autoloader)
    (update-autoloads-for-packages))
  (load-file loaddefs))

;;; Load all my custom lisp files.
(mapc 'load-library
      '("functions"
        "variables"
        "modes"
        "templates"
        "keys"))
