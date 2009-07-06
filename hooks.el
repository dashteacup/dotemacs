;; hooks.el
;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2009-06-09 16:07:28 pcurry>

;;; Description: Standard hooks for different modes.
;; Note that hooks can only contain function names not function calls.

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
            (local-set-key [f5] 'make-based-tests)))

(add-hook 'makefile-mode-hook
          (lambda ()
            (local-set-key "\C-cc" 'compile)))

(add-hook 'java-mode-hook
          (lambda ()
            (c-set-style "java")
            (c-toggle-hungry-state t)))

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

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; for emacs source code
            (setq tab-width 8)
            (eldoc-mode 1)))

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

(add-hook 'deeds-mode-hook
          (lambda ()
            (longlines-mode 1)
            (abbrev-mode 1)))

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
