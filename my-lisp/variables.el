;; variables.el
;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2018-07-18 21:01:57 pcurry>

;;; Description: Customization of various emacs variable settings.

(setq-default message-log-max 5000)

(setq mac-command-modifier 'meta)

;; Needed for OS X clipboard compatibility. May break other systems;
;; modify if it does.
(setq select-enable-clipboard t)

(when (featurep 'tool-bar)
  (tool-bar-mode -1))

(setq frame-title-format "%b")

;; Stop resizing my split windows
(setq even-window-heights nil)

;; Display specific customizations that I don't want on my Mac.
(unless (featurep 'aquamacs)
  ;; Show a horizontal line where the active cursor is.
  (when (require 'hl-line nil t)
    (global-hl-line-mode t)
    (set-face-background hl-line-face "azure2"))

  ;; Use a red bar for the cursor
  (add-to-list 'default-frame-alist '(cursor-color . "#ff0000"))
  (setq-default cursor-type 'bar)

  ;; Make the current window's mode line stand out more
  (set-face-background 'mode-line "light goldenrod"))

;; activates image viewing in emacs
(when (featurep 'image-file)
  (auto-image-file-mode t))

(setq default-major-mode 'text-mode)

;; Use the smarter ws-trim-mode when it exists instead of
;; delete-trailing-whitespace.
(when (srequire 'ws-trim)
  (global-ws-trim-mode t)
  (set-default 'ws-trim-level 0))

;; File beginning based mode selection gets confused too easily.
;(setq magic-mode-alist nil)

;; indicates lines that are empty like vi's ~
(setq-default indicate-empty-lines t)

;; automatically adds a newline to the end of a text file
(setq require-final-newline t)

;; Remember your place in files where that feature is useful.
(when (srequire 'saveplace)
  (setq-default save-place t)
  ;; Don't clutter my home dir with a ~/.emacs-places file
  (setq save-place-file "~/.emacs.d/saved-places"))

;; I use fboundp because the desktop feature exists in emacs 21 but
;; desktop-save-mode doesn't.
(when (fboundp 'desktop-save-mode)
  (desktop-save-mode 1)
  (setq desktop-save 'ask))

;; Reload file when it changes on disk
(global-auto-revert-mode t)

;; Enable non-gnu emacs package archives.
(when (srequire 'package)
  ;; Melpa pulls directly from a code repo, so it's less stable.
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  ;; Marmalade requires official package releases, so it's more stable.
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))

(setq user-mail-address "dashteacup@gmail.com")

(column-number-mode t)

;; no annoying beep noises
(setq visible-bell t)

;; put backups in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))

(unless (featurep 'aquamacs)
  (setq auto-save-file-name-transforms
        `((".*" "~/.emacs.d/auto-save-list/" t))))

;; use y/n instead of yes-or-no confirmation
(defalias 'yes-or-no-p 'y-or-n-p)

;; Add easier to remember command for turning wrapping on/off.
;; The name is similar to the vim command for turning off wrapping.
(defalias 'nowrap-toggle 'toggle-truncate-lines)

;; Sentence end commands will work with sentences that end with a single
;; space.
(setq sentence-end-double-space nil)

;; Enable window movement with Shift-<arrow-key>
(windmove-default-keybindings)

;; Use ido mode for switching between buffers and files.
(ido-mode 1)
(when (srequire 'ido-vertical-mode)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

;;; Enable disabled commands
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Scrolling with more context lines
(setq next-screen-context-lines 2)

;; Make dabbrev-expand work correctly with StudlyCaps
(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search nil)

(when (srequire 'subword)
  (global-subword-mode 1))

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

;;; Set bare minimums for undo limits
(let ((ulimit 50000))
  (when (< undo-limit ulimit)
    (setq undo-limit ulimit)))

(let ((uslimit 70000))
  (when (< undo-strong-limit uslimit)
    (setq undo-strong-limit uslimit)))

(auto-insert-mode)
(setq auto-insert t)
(setq auto-insert-query nil)
(setq auto-insert-directory "~/.insert/")

;; Set these values globally
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

(when (srequire 'evil)
  ;; Use the same type of cursor for insert/emacs states.
  (setq-default evil-emacs-state-cursor evil-insert-state-cursor)
  ;; Give me time to think after starting a search.
  (setq evil-flash-delay 60)
  ;; I want to start commit messages in insert state, but they don't have
  ;; their own mode. Try this for now.
  (add-to-list 'evil-insert-state-modes 'text-mode)
  (evil-mode 1))

;;; csv-mode settings
;; Don't hide the csv field separators (usually commas)
(setq csv-invisibility-default nil)
;; Since we show the separator, we don't need whitespace padding.
(setq csv-align-padding 0)
