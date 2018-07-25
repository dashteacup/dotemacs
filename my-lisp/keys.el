;; keys.el
;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2018-07-25 16:12:27 pcurry>

;;; Description: Global key bindings

(global-set-key "\C-cf" 'fill-region)

(global-set-key "\C-cs" 'ispell-buffer)

(global-set-key "\C-cr" 'comment-region)

;; read-only viewing mode
(global-set-key "\C-cv" 'view-mode)

(global-set-key "\M-n" 'scroll-up-command)
(global-set-key "\M-p" 'scroll-down-command)

(global-set-key "\M-gf" 'find-file-at-point)

;(global-set-key (kbd "C-<return>") 'complete-tag)

(global-set-key [?\C-\;] 'dabbrev-expand)

(global-set-key [f4] 'my-speedbar-get-focus)

(global-set-key [f5] 'revert-buffer-without-query)

(global-set-key [f6] 'delete-other-windows)

;; buffer-move package doesn't have a hook, so we bind globally
(when (srequire 'buffer-move)
  (global-set-key (kbd "<C-S-up>")    'buf-move-up)
  (global-set-key (kbd "<C-S-down>")  'buf-move-down)
  (global-set-key (kbd "<C-S-left>")  'buf-move-left)
  (global-set-key (kbd "<C-S-right>") 'buf-move-right))

(global-set-key "\M-`" 'other-frame)

(define-key ctl-x-4-map "k" 'kill-buffer-other-window)

;; use buffer-menu instead of list-buffers
(define-key ctl-x-map "\C-b" 'buffer-menu)

(define-key ctl-x-map "\C-v" 'view-file)

(define-key ctl-x-map "k" 'kill-this-buffer)

(define-key help-map "M" 'man)

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

(when (srequire 'evil)
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
    (define-key speedbar-mode-map "g" nil))
)
