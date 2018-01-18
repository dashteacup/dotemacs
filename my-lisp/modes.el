;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2018-01-18 17:39:52 currypx>

;;; Description: Configuration for many different modes.
;; Note that hooks can only contain function names not function calls.
;; I use anonymous functions to avoid unnecessary naming.

;;;; C/C++
;; use c++ mode by default for .h files
(add-to-list 'auto-mode-alist  '("\\.h\\'" . c++-mode))

;; use c++ mode on template definition files
(add-to-list 'auto-mode-alist  '("\\.def\\'" . c++-mode))

;; Use c++ for flex (aka lex) files.
(add-to-list 'auto-mode-alist '("\\.flex\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . c++-mode))

;; Use objective-c for .m files
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))

;; Use tcl-mode for .tk files
(add-to-list 'auto-mode-alist '("\\.tk\\'" . tcl-mode))

;; Setup magic mode so we can get modes for files without extensions.
(add-to-list 'magic-mode-alist '("#!/bin/bash" . sh-mode))
(add-to-list 'magic-mode-alist '("#!/bin/sh" . sh-mode))
(add-to-list 'magic-mode-alist '("#!/.*python" . python-mode))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "stroustrup")
            (setq c-tab-always-indent nil)
            (c-toggle-hungry-state t)
            (setq c-block-comment-prefix "")
            ; Use C++ style comments, which C has supported since c99.
            (setq comment-start "//")
            (setq comment-end "")
            (if (srequire 'subword) (subword-mode 1))
            (if (srequire 'flymake) (flymake-mode 1))
            (when (srequire 'flyspell) (flyspell-prog-mode))
            (local-set-key "\C-cc" 'compile)
            (local-set-key "\C-j" 'comment-indent-new-line)
            (local-set-key [f7] 'make-unit-tests)))

(add-hook 'makefile-mode-hook
          (lambda ()
            (local-set-key "\C-cc" 'compile)))

(add-hook 'java-mode-hook
          (lambda ()
            (when (srequire 'flyspell) (flyspell-prog-mode))
            (c-set-style "java")
            (c-toggle-hungry-state t)))

(add-hook 'python-mode-hook
          (lambda ()
            (if (srequire 'subword) (subword-mode 1))
            (when (srequire 'flyspell) (flyspell-prog-mode))
            (when (and (srequire 'flymake)
                       (exec-in-path-p "pylint"))
              (flymake-mode 1)
              (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)
              (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error))))

(add-hook 'cperl-mode-hook
          (lambda ()
            (cperl-set-style "PerlStyle")
            ;; Hide the stupid trailing whitespace underline.
            (setq cperl-invalid-face nil)
            (local-set-key "\C-hp" 'cperl-perldoc)
            (if (srequire 'flymake) (flymake-mode 1))))

(setq maude-command "/usr/local/maude-intelDarwin/maude")

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (when (srequire 'flyspell) (flyspell-prog-mode))
            ;; for emacs source code
            (setq tab-width 8)
            (eldoc-mode 1)))

(add-hook 'tcl-mode-hook
          (lambda ()
            (when (srequire 'flyspell) (flyspell-prog-mode))))

(add-hook 'sh-mode-hook
          (lambda ()
            (when (srequire 'flyspell) (flyspell-prog-mode))))

;; default doesn't work well for me
(if (featurep 'aquamacs) (setq text-mode-hook nil))
(add-hook 'text-mode-hook
          (lambda ()
            (when (featurep 'aquamacs)
              (smart-spacing-mode 1)
              ;; Need to update my console version of emacs so I can use this.
              (visual-line-mode 1))
            (when (srequire 'flyspell)
              ;; Need to set up ispell before I can use this in a terminal.
              (flyspell-mode 1))))

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

(defun my-org-mode-hook ()
  (setq evil-auto-indent nil))
(add-hook 'org-mode-hook 'my-org-mode-hook)

(add-hook 'asm-mode-hook
          (lambda ()
            (setq asm-comment-char 35)
            (local-set-key (kbd "<tab>") 'indent-relative)
            (local-set-key (kbd "<return>") 'newline)
            (local-set-key "#" 'self-insert-command)
            (local-set-key ":" 'self-insert-command)))

(add-hook 'write-file-hooks
          (lambda ()
            (unless (featurep 'ws-trim)
              (delete-trailing-whitespace))
            (time-stamp)
            (copyright-update)))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; Copied from: http://www.emacswiki.org/emacs/EmacsClient#toc36
;; Use C-x k to properly kill an emacsclient.
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))
;;; End Copy

(add-hook 'help-mode-hook
          (lambda ()
            (define-key help-mode-map "l" 'help-go-back)))

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key "\C-cc" 'compile)))

(add-hook 'flymake-mode-hook
          (lambda ()
            ;; I HATE dialog boxes.
            (setq flymake-gui-warnings-enabled nil)
            ;; wait these seconds before rerunning flymake
            (setq-default flymake-no-changes-timeout 1.5)))

;; flymake has no hook variable.  Annoying.
;; TODO: Yes it does.  Alter this code appropriately.
(when (load "flymake" t)
  ;;; Copied from: http://stackoverflow.com/questions/14082975/running-flymake-for-python-when-files-dont-have-py-extension
  ;; This lets me set flymake to run by filename OR mode
  (defun flymake-get-file-name-mode-and-masks (file-name)
    "Return the corresponding entry from `flymake-allowed-file-name-masks'."
    (unless (stringp file-name)
      (error "Invalid file-name"))
    (let ((fnm flymake-allowed-file-name-masks)
          (mode-and-masks nil)
          (matcher nil))
      (while (and (not mode-and-masks) fnm)
        (setq matcher (car (car fnm)))
        (if (or (and (stringp matcher) (string-match matcher file-name))
                (and (symbolp matcher) (equal matcher major-mode)))
            (setq mode-and-masks (cdr (car fnm))))
        (setq fnm (cdr fnm)))
      (flymake-log 3 "file %s, init=%s" file-name (car mode-and-masks))
      mode-and-masks))
  ;;; End Copy

  (defvar epylintargs "-d W0403,W0232,R,C"
    "String of command line arguments to be passed to pylint.")

  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace)))
      (list "epylint" (list temp-file epylintargs))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '(python-mode flymake-pylint-init))
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
