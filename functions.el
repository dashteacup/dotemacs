;; functions.el
;; Author: Paul Curry
;; Created: 2006-12-09
;; Time-stamp: <2009-07-06 14:18:44 pcurry>

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
