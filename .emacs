;;;; Paul's .emacs file
;; Originally written sometime before September 7, 2003
;; Time-stamp: <2013-07-04 22:22:54 pcurry>

;;; Note: This should be in the user's home directory (~).
;;; Or you could just make ~/.emacs a symbolic link to this file.

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
