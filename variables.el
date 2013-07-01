;; variables.el
;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2013-05-16 15:26:20 pcurry>

;;; Description: Customization of various emacs variable settings.

(when (boundp 'mac-command-modifier)
  (setq mac-command-modifier 'meta))

;; Needed for OS X clipboard compatibility. May break other systems;
;; modify if it does.
(setq x-select-enable-clipboard t)

(when (featurep 'tool-bar)
  (tool-bar-mode -1))

(setq frame-title-format "%b")

;; activates image viewing in emacs
(when (featurep 'image-file)
  (auto-image-file-mode t))

(setq default-major-mode 'text-mode)

;; File beginning based mode selection gets confused too easily.
(setq magic-mode-alist nil)

;; indicates lines that are empty like vi's ~
(setq default-indicate-empty-lines t)

;; automatically adds a newline to the end of a text file
(setq require-final-newline t)

(setq user-mail-address "pcurry2@illinois.edu")

(setq gnus-select-method '(nntp "news.cs.uiuc.edu"))

(column-number-mode t)

;; no annoying beep noises
(setq visible-bell t)

;; put backups in one place
(setq backup-directory-alist '(("." . "~/.emacs_backups")))

;; use y/n instead of yes-or-no confirmation
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Enable disabled commands
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Scrolling with more context lines
(setq next-screen-context-lines 2)

;; Make dabbrev-expand work correctly with StudlyCaps
(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search nil)

;; dired shouldn't show hidden files
(setq dired-listing-switches "-l")

;; use aspell instead of ispell
(unless (or (string-match "cygwin" (emacs-version))
            ;; 2.0+ uses built in mac os spellcheck
            (featurep 'aquamacs))
  (if (exec-in-path-p "aspell")
      (setq-default ispell-program-name "aspell")))

(show-paren-mode t)

;; case insensitive
(setq case-fold-search t)

(setq confirm-kill-emacs nil)

;;; indentation settings
(setq standard-indent 4)
(setq tab-always-indent nil)
(setq-default tab-width 4)
;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(setq transient-mark-mode t)

(setq undo-limit 50000)

(setq undo-strong-limit 70000)

(auto-insert-mode)
(setq auto-insert t)
(setq auto-insert-query nil)
(setq auto-insert-directory "~/.insert/")

