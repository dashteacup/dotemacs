;; keys.el
;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2018-07-31 14:33:30 pcurry>

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
