;; mode-load.el
;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2008-06-06 10:00:24 pcurry>

;;; Description: Sets up autoload and modes for files.

;; use c++ mode by default for .h files
(add-to-list 'auto-mode-alist  '("\\.h\\'" . c++-mode))

;; use c++ mode on template definition files
(add-to-list 'auto-mode-alist  '("\\.def\\'" . c++-mode))

;; use nxml-mode if available, override magic data-specific setttings
(load "~/.elisp/site-lisp/nxml-mode/rng-auto.el" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(xml\\|xsl\\|rng\\|x?html\\)\\'" . nxml-mode))

(autoload 'css-mode "css-mode-simple" nil t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;; Use the emacs 22 python-mode if possible.
(unless (fboundp 'python-mode)
  (autoload 'python-mode "python-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode)))

(unless (fboundp 'php-mode)
  (autoload 'php-mode "php-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode)))

(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;; clipped from append-tuareg.el
(add-to-list 'auto-mode-alist  '("\\.ml\\w?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" nil t)
(autoload 'camldebug "camldebug-tuareg" nil t)

(if (and (boundp 'window-system) window-system)
    (when (string-match "Emacs" emacs-version)
       	(if (not (and (boundp 'mule-x-win-initted) mule-x-win-initted))
            (require 'sym-lock))
       	(require 'font-lock)))
;; end clip

(autoload 'maude-mode "maude-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.maude\\'" . maude-mode))
(setq maude-command "/usr/local/maude-intelDarwin/maude")

(add-to-list 'auto-mode-alist '("\\.deeds\\'" . deeds-mode))
