;; functions.el
;; Author: Paul Curry
;; Created: 2006-12-09
;; Time-stamp: <2009-06-18 17:24:48 pcurry>

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

(defun make-based-tests ()
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

(defmacro srequire (feature)
  "Silent `require'.  Shorthand for (require feature nil t).
Requires feature while surpressing error messages.
Return feature if feature can be loaded, nil otherwise."
  `(require ,feature nil t))


;;; Advice for existing functions

(defadvice dired-up-directory (around delete-when-going-up
                                      nil
                                      activate)
  "As `dired-up-directory', but kills the current directory's buffer."
  (let ((dir (current-buffer)))
    ad-do-it
    (kill-buffer dir)))

(defadvice just-one-space (before two-spaces-after-sentence
                                  nil
                                  activate)
  "Delete all but two spaces if at the start of a sentence."
  (if (and (looking-back (sentence-end))
           (ad-get-arg 0))
      (ad-set-arg 0 2)))


;;; New modes

;; deeds-mode macros

(defmacro defwdeeds (name arglist docstring &rest body)
  "Define a widening interactive function for `deeds-mode'.
A widening function shows all deeds list entries before performing any
other actions."
 (declare (indent defun))
  `(defun ,name ,arglist
     ,docstring
     (interactive)
     (deeds-show-all-entries)
     ,@body))


;; deeds-mode user functions

(defun deeds-newline ()
  "End an item on the day's deed list."
  (interactive)
  (move-end-of-line 1)
  (delete-horizontal-space)
  (unless (or (looking-back "\\.")
              (looking-back "^")) ; work normal on blank lines
    (insert "."))
  (newline))

(defwdeeds deeds-new-entry ()
  "Start a new deed list for today."
  (if (save-excursion (not (or (re-search-backward deeds-header nil t)
                               (re-search-forward deeds-header nil t))))
      (goto-char (point-min))
    (goto-char (point-max))
    (if (not (looking-back "\\.[[:space:]]*"))
        (progn (re-search-backward "\\w")
               (goto-char (match-end 0))
               (insert ".")
               (newline 2))
      (re-search-backward "\\.")
      (forward-char)
      (delete-region (point) (point-max))
      (newline 2)))
  (deeds-insert-entry-header))

(defwdeeds deeds-only-this-entry ()
  "Narrow the buffer to show only the entry at point."
  (save-excursion
    (if (deeds-in-header-p)
        (forward-line (- 3 (deeds-in-header-p))))
    (let ((begin (save-excursion (re-search-backward deeds-header nil t)))
          (end (save-excursion
                 (when (re-search-forward deeds-header nil 1)
                   ;; put end on blank line before next header
                   (goto-char (match-beginning 0))
                   (backward-char))
                 (point))))
      (if begin
          (narrow-to-region begin end)
        (message "The point isn't in a deeds list entry.")))))

(defwdeeds deeds-only-recent-entry ()
  "Narrow the buffer to show only the most recent day's entry."
  ;; check if inside last entry
  (if (re-search-forward deeds-header nil t)
      (goto-char (point-max)))
  (deeds-only-this-entry))

(defalias 'deeds-show-all-entries 'widen
  "Widen the buffer to show all the entries.
Same effect as the `widen' function.")

(define-derived-mode deeds-mode text-mode "Deeds"
  "Major mode for editing deeds files.
A deeds file (extension .deeds) provides a list of a daily
activities.  Each entry in the deeds file is seperated with 50
-'s and the date.
\\{deeds-mode-map}"
  (define-key deeds-mode-map (kbd "<return>") 'deeds-newline)
  (define-key deeds-mode-map "\C-c\C-e" 'deeds-new-entry)
  (define-key deeds-mode-map "\C-c\C-t" 'deeds-only-this-entry)
  (define-key deeds-mode-map "\C-c\C-r" 'deeds-only-recent-entry)
  (define-key deeds-mode-map "\C-c\C-a" 'deeds-show-all-entries))


;; deeds-mode internal functionality

(defconst deeds-header-1
  "^-\\{50\\}$"
  "Regexp for the 1st line of a deeds list entry's header.")

(defconst deeds-header-2
  (let ((d "[[:digit:]]"))
    (concat "^" d "\\{4\\}-" d "\\{2\\}-" d "\\{2\\}$"))
  "Regexp for the 2nd line of a deeds list entry's header.")

(defconst deeds-header
  (concat (substring deeds-header-1 0 -1)
          "\n"
          (substring deeds-header-2 1))
  "Regexp for the header of a deed list entry in `deeds-mode'.")

(defun deeds-in-header-p ()
  "Determine if the point is currently in the header of a deeds list entry.
Returns 1 if the point is on the first line of the header, 2 for
the second, and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at deeds-header-2)
           (previous-line)
           (if (looking-at deeds-header) 2))
          ((looking-at deeds-header) 1))))

(defun deeds-insert-entry-header ()
  "Inserts the header for today's deed list."
  (insert-char (string-to-char "-") 50)
  (newline)
  (insert (iso-date))
  (newline 2))
