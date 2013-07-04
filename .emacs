;;;; Paul's .emacs file
;; Originally written sometime before September 7, 2003
;; Time-stamp: <2013-07-04 14:46:34 pcurry>

;;; Note: This should be in the user's home directory (~).
;;; Or you could just make ~/.emacs a symbolic link to this file.

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
