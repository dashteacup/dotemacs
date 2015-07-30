;; keys.el
;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2015-07-30 12:09:04 currypx>

;;; Description: Global key bindings

(global-set-key "\C-cf" 'fill-region)

(global-set-key "\C-cs" 'ispell-buffer)

(global-set-key "\C-cr" 'comment-region)

(global-set-key "\C-cl" 'count-lines-region)

;; read-only viewing mode
(global-set-key "\C-cv" 'view-mode)

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
