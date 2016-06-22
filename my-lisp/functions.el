;; functions.el
;; Author: Paul Curry
;; Created: 2006-12-09
;; Time-stamp: <2016-06-21 15:09:23 currypx>

;;; Description: Helper function definitions.

(defun rename (filename)
  "Change the current file's name."
  (interactive "FNew name: ")
  (let ((oldname))
    (if (and (buffer-file-name)
             (file-exists-p (buffer-file-name)))
        (setq oldname (buffer-file-name)))
    (write-file filename)
    (if oldname (delete-file oldname))))

(defun kill-buffer-other-window ()
  "Kill the buffer in the other window.
If the frame has more than two windows kill the next buffer in
cyclic order.  See `other-window'."
  (interactive)
  (other-window 1)
  (kill-buffer nil)
  (other-window -1))

(defun revert-buffer-without-query ()
  "Revert buffer without confirmation.
Replace current buffer text with the text of the file on disk.
This is like `revert-buffer' but without asking whether you
really want to reload the file. This is handy if you are swapping
between using emacs and another tool (like an IDE) to edit a
file."
  (interactive)
  (revert-buffer t t))

;; No longer necessary in Aquamacs, unsure elsewhere.
(defun longlines-heuristic ()
  "Determine if the buffer contains long lines.
Return t if any of the first 100 lines are longer than 80
characters."
  (save-excursion
    (goto-char (point-min))
    (let (beg over80)
      (dotimes (i 100)
        (beginning-of-line)
        (setq beg (point))
        (end-of-line)
        (if (> (- (point) beg) 80)
            (setq over80 t)
          (forward-line)))
      over80)))

(defun exec-in-path-p (program)
  "Return t when PROGRAM is in the path.
If PROGRAM is not in the path, or this information cannot be
determined, return nil instead.

Warning: Runs the PROGRAM (with no arguments) in its heuristics.
Only call it on safe programs."
  (condition-case nil
      (progn (call-process program nil nil nil) t)
    (error nil)))

(defun make-unit-tests ()
  "Run unit tests that are activated with 'make check'"
  (interactive)
  (compile "make check"))

(defun spaces-tabs-common ()
  "Shared code between `use-spaces' and `use-tabs'."
  (unless num (setq num 4)) ; dynamic scoping
  (setq standard-indent num)
  (setq tab-width num))

(defun use-spaces (&optional num)
  "Use only spaces for indentation.
Uses 4 space indentation by default."
  (interactive "P")
  (spaces-tabs-common)
  (setq indent-tabs-mode nil))

(defun use-tabs (&optional num)
  "Use tabs for indentation.
Aligns tabs a 4 spaces by default."
  (interactive "P")
  (spaces-tabs-common)
  (setq indent-tabs-mode t))

(defun iso-date (&optional time)
  "Return the date at TIME in ISO format, or today's date if omitted.
TIME has the same format as in `format-time-string'.
The ISO format is YYYY-MM-DD."
  (format-time-string "%Y-%m-%d" (or time (current-time))))

(defun srequire (feature)
  "Silent `require'.  Shorthand for (require feature nil t).
Requires feature while surpressing error messages.
Return feature if feature can be loaded, nil otherwise."
  (require feature nil t))

(defun my-speedbar-get-focus ()
  "Change frame focus to or from the speedbar frame.
This customized version of `speedbar-get-focus' works the same except it
doesn't refresh the frame when you switch"
  (interactive)
  (speedbar-reset-scanners)
  (dframe-get-focus 'speedbar-frame 'speedbar-frame-mode))


;;; Advice for existing functions

(defadvice dired-up-directory (around delete-when-going-up
                                      nil
                                      activate)
  "As `dired-up-directory', but kills the current directory's buffer."
  (let ((dir (current-buffer)))
    ad-do-it
    (kill-buffer dir)))

;;; Code is swiped from: https://snarfed.org/dotfiles/.emacs
;; VC backup files are different from normal backup files. they're used e.g.
;; when you open an old revision of a file (like from *vc-change-log*). they're
;; written to the current dir by default. put them in /tmp instead.
(defadvice vc-version-backup-file-name (after use-tmp-dir activate)
  (setq ad-return-value (expand-file-name (file-name-nondirectory ad-return-value)
                                          temporary-file-directory)))
;;; end swipe

;; Make * and # behave like they do vim (underscore and hyphen are part of a
;; single word)
(defadvice evil-search-word-forward (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    (with-syntax-table table
      ad-do-it)))

(defadvice evil-search-word-backward (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    (with-syntax-table table
      ad-do-it)))

;; After using * or # you need to treat underscore and hyphen as part of a
;; word for n and N to work properly.
(defadvice evil-search-next (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    (with-syntax-table table
      ad-do-it)))

(defadvice evil-search-previous (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    (with-syntax-table table
      ad-do-it)))

;;; Commented out because I've been using a single space between sentences
;;; lately.
;; (defadvice just-one-space (before two-spaces-after-sentence
;;                                   nil
;;                                   activate)
;;   "Delete all but two spaces if at the start of a sentence."
;;   (if (and (looking-back (sentence-end))
;;            (ad-get-arg 0))
;;       (ad-set-arg 0 2)))
