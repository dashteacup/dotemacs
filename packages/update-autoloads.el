;; Author: Paul Curry
;; Created: 2013-07-03
;; Time-stamp: <2013-07-04 14:22:14 pcurry>

;;; Description: Make magic autoloads work for user installed .el files.

;;; Installation:
;; Place this file in your user installed package directory. For me it's
;; ~/.emacs.d/packages. Run the `update-autoloads-for-packages' function to
;; generate a loaddefs.el in your package directory. Then, load the
;; loaddefs.el from you .emacs file.  I use
;; (load-file "~/.emacs.d/packages/loaddefs.el")

;; Modified from: http://stackoverflow.com/questions/4189159/emacs23-elisp-how-to-properly-autoload-this-library

;;;###autoload
(defun update-autoloads-for-packages ()
  "Update loaddefs.el with the magic autoloads found in my package directory.
Creates a new loaddefs.el using the magic autoloads (;;;###autoload)
found in the files in you package directory. The loaddefs.el is
located in the same directory."
  (interactive)
  (let ((package-directory (file-truename
                            (file-name-directory
                             (symbol-file 'update-autoloads-for-packages 'defun)))))
    (require 'autoload)
    ;; Use dynamic binding to change location of loaddefs
    ;; (the generate-autoload-file variable) used by update-file-autoloads
    (let ((generated-autoload-file (concat package-directory "loaddefs.el")))
      (update-directory-autoloads package-directory))))
