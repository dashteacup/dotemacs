;; keys.el
;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2016-06-06 12:47:37 currypx>

;;; Description: Global key bindings

(global-set-key "\C-cf" 'fill-region)

(global-set-key "\C-cs" 'ispell-buffer)

(global-set-key "\C-cr" 'comment-region)

;; Emacs 24.1 added count-words-region bound to M-=
;(global-set-key "\C-cl" 'count-lines-region)

;; read-only viewing mode
(global-set-key "\C-cv" 'view-mode)

(global-set-key "\M-n" 'scroll-up-command)
(global-set-key "\M-p" 'scroll-down-command)

(global-set-key "\M-gf" 'find-file-at-point)

;(global-set-key (kbd "C-<return>") 'complete-tag)

(global-set-key [?\C-\;] 'dabbrev-expand)

(global-set-key [f5] 'revert-buffer-without-query)

(global-set-key [f6] 'delete-other-windows)

(define-key ctl-x-4-map "k" 'kill-buffer-other-window)

;; use buffer-menu instead of list-buffers
(define-key ctl-x-map "\C-b" 'buffer-menu)

(define-key ctl-x-map "\C-v" 'view-file)

(define-key ctl-x-map "k" 'kill-this-buffer)

(define-key help-map "M" 'man)

(when (srequire 'help-mode)
  (define-key help-mode-map "o" 'other-window)
  (define-key help-mode-map "n" 'next-line)
  (define-key help-mode-map "p" 'previous-line)
  ;; vim-ish line movement (slightly easier to type)
  (define-key help-mode-map "j" 'next-line)
  (define-key help-mode-map "k" 'previous-line))

(when (srequire 'view)
  (define-key view-mode-map "n" 'next-line)
  (define-key view-mode-map "p" 'previous-line)
  (define-key view-mode-map "j" 'next-line)
  (define-key view-mode-map "k" 'previous-line))

(define-key messages-buffer-mode-map "o" 'other-window)
(define-key messages-buffer-mode-map "n" 'next-line)
(define-key messages-buffer-mode-map "p" 'previous-line)
(define-key messages-buffer-mode-map "j" 'next-line)
(define-key messages-buffer-mode-map "k" 'previous-line)

(defun ido-my-keys ()
  "Add my personal keybindings to ido.
ido-mode expects you to add custom keybindings by adding hooks to ido-setup-hook."
  ;; Make backward-kill-word keybinding work the way I expect it to.
  (define-key ido-completion-map (kbd "<M-backspace>") 'ido-delete-backward-updir))
(add-hook 'ido-setup-hook 'ido-my-keys)

(when (srequire 'evil)
  ;; I prefer space/backspace to scroll like in view-mode
  (define-key evil-normal-state-map (kbd "SPC") 'evil-scroll-page-down)
  (define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-page-up)

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
)
