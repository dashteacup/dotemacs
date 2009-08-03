;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2009-08-03 11:42:14 pcurry>

;;; Description: Configuration for many different modes.
;; Note that hooks can only contain function names not function calls.
;; I use anonymous functions to avoid unnecessary naming.

;;;; C/C++
;; use c++ mode by default for .h files
(add-to-list 'auto-mode-alist  '("\\.h\\'" . c++-mode))

;; use c++ mode on template definition files
(add-to-list 'auto-mode-alist  '("\\.def\\'" . c++-mode))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "stroustrup")
            (setq c-tab-always-indent nil)
            (c-toggle-hungry-state t)
            (setq c-block-comment-prefix "")
            (if (srequire 'cc-subword) (c-subword-mode 1))
            (if (srequire 'flymake) (flymake-mode 1))
            (local-set-key "\C-cc" 'compile)
            (local-set-key "\C-j" 'comment-indent-new-line)
            (local-set-key [f5] 'make-unit-tests)))



(add-hook 'makefile-mode-hook
          (lambda ()
            (local-set-key "\C-cc" 'compile)))

(add-hook 'java-mode-hook
          (lambda ()
            (c-set-style "java")
            (c-toggle-hungry-state t)))

;;;; Python
;; Use the emacs 22 python-mode if possible.
(unless (fboundp 'python-mode)
  (autoload 'python-mode "python-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode)))

(add-hook 'python-mode-hook
          (lambda ()
            (if (srequire 'cc-subword) (c-subword-mode 1))
            (if (and (srequire 'flymake)
                     (exec-in-path-p "pylint"))
                (flymake-mode 1))))



(add-hook 'cperl-mode-hook
          (lambda ()
            (cperl-set-style "PerlStyle")
            (local-set-key "\C-hp" 'cperl-perldoc)
            (if (srequire 'flymake) (flymake-mode 1))))

;;;; OCaml
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

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; for emacs source code
            (setq tab-width 8)
            (eldoc-mode 1)))

;;; Web modes
;; use nxml-mode if available, override magic data-specific setttings
(load "~/.elisp/site-lisp/nxml-mode/rng-auto.el" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(xml\\|xsl\\|rng\\|x?html\\)\\'" . nxml-mode))

(unless (fboundp 'php-mode)
  (autoload 'php-mode "php-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode)))



(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode 1)
            (if (and (srequire 'longlines) (longlines-heuristic))
                (longlines-mode 1))))

;; auctex
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-PDF-mode t)
            (setq TeX-output-view-style
                  (cons '("^pdf$" "." "open %o") TeX-output-view-style))
            (local-set-key "\C-cc"
                           (lambda ()
                             (interactive)
                             (save-buffer)
                             (TeX-command TeX-command-default
                                          'TeX-master-file)))
            (add-to-list 'font-latex-quote-list '("``" "\""))
            (setq skeleton-pair t)
            (mapc (lambda (char)
                    (local-set-key char 'skeleton-pair-insert-maybe))
                  '("(" "[" "{"))))

(add-hook 'asm-mode-hook
          (lambda ()
            (setq asm-comment-char 35)
            (local-set-key (kbd "<tab>") 'indent-relative)
            (local-set-key (kbd "<return>") 'newline)
            (local-set-key "#" 'self-insert-command)
            (local-set-key ":" 'self-insert-command)))

(add-hook 'write-file-hooks
          (lambda ()
            (delete-trailing-whitespace)
            (time-stamp)
            (copyright-update)))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-hook 'help-mode-hook
          (lambda ()
            (define-key help-mode-map "l" 'help-go-back)))

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key "\C-cc" 'compile)))

(add-hook 'flymake-mode-hook
          (lambda ()
            ;; I HATE dialog boxes.
            (setq flymake-gui-warnings-enabled nil)))

;; flymake has no hook variable.  Annoying.
;; TODO: Yes it does.  Alter this code appropriately.
(when (load "flymake" t)
  (defvar epylintargs ""
    "String of command line arguments to be passed to pylint.")

  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint.py" (list local-file epylintargs))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.pm\\'" flymake-perl-init)))


;; flymake for javascript mode
(defconst flymake-allowed-js-file-name-masks '(("\\.json$" flymake-js-init)
                                               ("\\.js$" flymake-js-init)))
(defcustom flymake-js-detect-trailing-comma t nil :type 'boolean)
(defvar flymake-js-err-line-patterns
  '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" 1 2 nil 3)))
(when flymake-js-detect-trailing-comma
  (setq flymake-js-err-line-patterns
        (append flymake-js-err-line-patterns
                '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(strict warning: trailing comma.+\\)\:$" 1 2 nil 3)))))

(defun flymake-js-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "js" (list "-s" local-file))))
(defun flymake-js-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks
        (append flymake-allowed-file-name-masks flymake-allowed-js-file-name-masks))
  (setq flymake-err-line-patterns flymake-js-err-line-patterns)
  (flymake-mode t))

(add-hook 'javascript-mode-hook '(lambda () (flymake-js-load)))
