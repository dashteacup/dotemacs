;;;; Paul's init.el file
;; Originally written sometime before September 7, 2003
;; Time-stamp: <2013-07-14 22:43:09 pcurry>

;;; This used to be my .emacs file, but Emacs 22 and up allows you to
;;; use ~/.emacs.d/init.el as your config file, so it got a name change.

;; These paths are constants.
(let* ((elisp-dir "~/.emacs.d/")
       (package-dir (concat elisp-dir "packages/"))
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
