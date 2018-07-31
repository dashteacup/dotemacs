;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2018-07-31 15:25:01 pcurry>

;;; Description: Configuration for many different modes.
;; Note that hooks can only contain function names not function calls.

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

(setq maude-command "/usr/local/maude-intelDarwin/maude")

(defun my-c-mode-common-hook ()
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
  (local-set-key [f7] 'make-unit-tests))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


(defun my-makefile-mode-hook ()
  (local-set-key "\C-cc" 'compile))
(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)


(defun my-java-mode-hook ()
  (when (srequire 'flyspell) (flyspell-prog-mode))
  (c-set-style "java")
  (c-toggle-hungry-state t))
(add-hook 'java-mode-hook 'my-java-mode-hook)


(defun my-python-mode-hook ()
  (if (srequire 'subword) (subword-mode 1))
  (when (srequire 'flyspell) (flyspell-prog-mode))
  (when (and (srequire 'flymake)
             (exec-in-path-p "pylint"))
    (flymake-mode 1)
    (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)
    (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))
(add-hook 'python-mode-hook 'my-python-mode-hook)


(defun my-cperl-mode-hook ()
  (cperl-set-style "PerlStyle")
  ;; Hide the stupid trailing whitespace underline.
  (setq cperl-invalid-face nil)
  (local-set-key "\C-hp" 'cperl-perldoc)
  (if (srequire 'flymake) (flymake-mode 1)))
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)


(defun my-emacs-lisp-mode-hook ()
  (when (srequire 'flyspell) (flyspell-prog-mode))
  ;; for emacs source code
  (setq tab-width 8)
  (eldoc-mode 1))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)


(defun my-tcl-mode-hook ()
  (when (srequire 'flyspell) (flyspell-prog-mode)))
(add-hook 'tcl-mode-hook 'my-tcl-mode-hook)


(defun my-sh-mode-hook ()
  (when (srequire 'flyspell) (flyspell-prog-mode)))
(add-hook 'sh-mode-hook 'my-sh-mode-hook)


(defun my-text-mode-hook ()
  (when (featurep 'aquamacs)
    (smart-spacing-mode 1)
    ;; Need to update my console version of emacs so I can use this.
    (visual-line-mode 1))
  (when (srequire 'flyspell)
    ;; Need to set up ispell before I can use this in a terminal.
    (flyspell-mode 1)))
;; default doesn't work well for me
(if (featurep 'aquamacs) (setq text-mode-hook nil))
(add-hook 'text-mode-hook 'my-text-mode-hook)


;; auctex
(defun my-LaTeX-mode-hook ()
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
        '("(" "[" "{")))
(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)


(defun my-org-mode-hook ()
  (setq evil-auto-indent nil))
(add-hook 'org-mode-hook 'my-org-mode-hook)


(defun my-asm-mode-hook ()
  (setq asm-comment-char 35)
  (local-set-key (kbd "<tab>") 'indent-relative)
  (local-set-key (kbd "<return>") 'newline)
  (local-set-key "#" 'self-insert-command)
  (local-set-key ":" 'self-insert-command))
(add-hook 'asm-mode-hook 'my-asm-mode-hook)


(defun my-evil-after-load-hook ()
  ;; I prefer space/backspace to scroll like in view-mode
  (define-key evil-normal-state-map (kbd "SPC") 'evil-scroll-page-down)
  (define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-page-up)
  (define-key evil-motion-state-map (kbd "SPC") 'evil-scroll-page-down)
  (define-key evil-motion-state-map (kbd "<backspace>") 'evil-scroll-page-up)
  ;; I prefer emacs' find-tag over evil-repeat-pop
  (define-key evil-normal-state-map "\M-." 'find-tag)

  ;; Unbind all of these insert mode shortcuts so it will behave like normal emacs
  (define-key evil-insert-state-map "\C-w" nil) ; evil-delete-backward-word
  (define-key evil-insert-state-map "\C-a" nil) ; evil-paste-last-insertion
  (define-key evil-insert-state-map "\C-d" nil) ; evil-shift-left-line
  (define-key evil-insert-state-map "\C-t" nil) ; evil-shift-right-line
  (define-key evil-insert-state-map "\C-p" nil) ; evil-complete-previous
  (define-key evil-insert-state-map "\C-n" nil) ; evil-complete-next
  (define-key evil-insert-state-map "\C-e" nil) ; evil-copy-from-below
  (define-key evil-insert-state-map "\C-y" nil) ; evil-copy-from-above
  (define-key evil-insert-state-map "\C-r" nil) ; evil-paste-from-register
  (define-key evil-insert-state-map "\C-o" nil) ; evil-execute-in-normal-state
  (define-key evil-insert-state-map "\C-k" nil) ; evil-insert-digraph
  (define-key evil-insert-state-map "\C-v" nil) ; quoted-insert
  ;(define-key evil-insert-state-map "\C-z" nil) ; evil-emacs-state
  (define-key evil-insert-state-map (kbd "<delete>") nil)  ; delete-char

  ;; I want to be able to use tab to move between links in help-mode.
  (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
  (evil-define-key 'motion help-mode-map (kbd "SPC") 'scroll-up-command)
  (evil-define-key 'motion help-mode-map (kbd "<backspace>") 'scroll-down-command)
  ;; I want evil to go to the top of the speedbar with gg instead of
  ;; refreshing the buffer. You can still refresh it with r.
  (when (srequire 'speedbar)
    (define-key speedbar-mode-map "g" nil)))
(add-hook 'evil-after-load-hook 'my-evil-after-load-hook)


(defun my-write-file-hooks ()
  (unless (featurep 'ws-trim)
    (delete-trailing-whitespace))
  (time-stamp)
  (copyright-update))
(add-hook 'write-file-hooks 'my-write-file-hooks)


(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(defun my-ido-setup-hook ()
  "ido-mode uses the non-standard hook name ido-setup-hook."
  ;; Make backward-kill-word keybinding work the way I expect it to.
  (define-key ido-completion-map (kbd "<M-backspace>")
                                 'ido-delete-backward-updir))
(add-hook 'ido-setup-hook 'my-ido-setup-hook)


;;; Copied from: http://www.emacswiki.org/emacs/EmacsClient#toc36
;; Use C-x k to properly kill an emacsclient.
(defun my-server-switch-hook ()
  (when (current-local-map)
    (use-local-map (copy-keymap (current-local-map))))
  (when server-buffer-clients
    (local-set-key (kbd "C-x k") 'server-edit)))
;;; End Copy
(add-hook 'server-switch-hook 'my-server-switch-hook)


(defun my-help-mode-hook ()
  (define-key help-mode-map "l" 'help-go-back)
  (define-key help-mode-map "o" 'other-window)
  (define-key help-mode-map "n" 'next-line)
  (define-key help-mode-map "p" 'previous-line)
  ;; vim-ish line movement (slightly easier to type)
  (define-key help-mode-map "j" 'next-line)
  (define-key help-mode-map "k" 'previous-line))
(add-hook 'help-mode-hook 'my-help-mode-hook)


(defun my-messages-buffer-mode-hook ()
  (define-key messages-buffer-mode-map "o" 'other-window)
  (define-key messages-buffer-mode-map "n" 'next-line)
  (define-key messages-buffer-mode-map "p" 'previous-line)
  (define-key messages-buffer-mode-map "j" 'next-line)
  (define-key messages-buffer-mode-map "k" 'previous-line))
;; The hook won't apply to the *Messages* buffer created at startup
;; because it already exists by the time the user customization file
;; is loaded. Get around this by calling it manually the first time.
(with-current-buffer "*Messages*" (my-messages-buffer-mode-hook))
;; Add hook so it still works after killing the *Messages* buffer.
(add-hook 'messages-buffer-mode-hook 'my-messages-buffer-mode-hook)


(defun my-dired-mode-hook ()
  (local-set-key "\C-cc" 'compile))
(add-hook 'dired-mode-hook 'my-dired-mode-hook)


(defun my-view-mode-hook ()
  (define-key view-mode-map "n" 'next-line)
  (define-key view-mode-map "p" 'previous-line)
  (define-key view-mode-map "j" 'next-line)
  (define-key view-mode-map "k" 'previous-line))
(add-hook 'view-mode-hook 'my-view-mode-hook)


(defun my-flyspell-mode-hook ()
  ;; This is reserved for my dabbrev-expand key binding.
  (define-key flyspell-mode-map [?\C-\;] nil))
(add-hook 'flyspell-mode-hook 'my-flyspell-mode-hook)


(defun my-flymake-mode-hook ()
  ;; I HATE dialog boxes.
  (setq flymake-gui-warnings-enabled nil)
  ;; wait these seconds before rerunning flymake
  (setq-default flymake-no-changes-timeout 1.5))
(add-hook 'flymake-mode-hook 'my-flymake-mode-hook)


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
(add-hook 'javascript-mode-hook 'flymake-js-load)
