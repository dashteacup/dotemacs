;;; notes.el
;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2016-08-23 10:46:44 currypx>

;; Description: Notes and hints I've collected over the years.


;; To type a newline in an interactive buffer use C-q C-j
;; quoted-insert then newline.  This is helpful when doing a
;; replace-regexp.

;; To indent a large region use C-x TAB (indent-rigidly).  Useful for
;; stubborn comment blocks.

;; The commands M-/ and C-M-/ do code completion based on what is
;; written in open buffers.

;; C-u C-SPC jumps back to last marked position, and C-x C-x jumps to
;; the other end of a marked region.

;; M-% is the shortcut for query replace.

;; M-! runs a shell command and displays its output in the minibuffer.
;; C-u M-! runs does the same but displays the output in the current
;; buffer.

;; M-m moves the point to the first non-whitespace character on the
;; line.

;; C-M-n is the forward list command.  It's useful for editing lisp
;; code.

;; Learn keyboard macros: C-x C-( starts recording, C-x C-) ends,
;; name-last-kbd-macro names a macro, and insert-kbd-macro generates
;; lisp code for the macro.

;; Rectangle and register commands (C-x r ?).

;; Frames (C-x 5 ?).

;; set-selective-display (C-x $) for seeing fun/class defn

;; tags: M-. (find) M-* (pop back)

;; longlines-mode useful for viewing text files with soft newlines.

;; dired-do-query-replace-regexp <- useful!

;; Show human readable description of key code number X:
;; (key-description [X])
