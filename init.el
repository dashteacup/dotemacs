;;;; Paul's init.el file
;; Originally written sometime before September 7, 2003
;; Time-stamp: <2018-08-01 18:22:45 pcurry>

;;; This used to be my .emacs file, but Emacs 22 and up allows you to
;;; use ~/.emacs.d/init.el as your config file, so it got a name change.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck markdown-mode evil buffer-move csv-mode ws-trim ido-vertical-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Necessary to be able to require installed packages.
(package-initialize)

(let ((my-elisp-dir "~/.emacs.d/my-lisp/"))
  (add-to-list 'load-path my-elisp-dir))

;;; Load all my custom lisp files.
(mapc 'load-library
      '("functions"
        "modes"
        "variables"
        "templates"
        "keys"))
