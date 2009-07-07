;; templates.el
;; Author: Paul Curry
;; Created: 2006-10-27
;; Time-stamp: <2009-07-07 13:51:45 pcurry>

;;; Description: Skeletons and related functions for auto-insert.

(defun prepend-comment (comm source)
  "Return SOURCE with comment string COMM prepended to every line."
  (let* ((strings (split-string source "\n"))
         (result (mapconcat (lambda (str) (concat comm str)) strings "\n")))
    (if (equal (car (last strings)) "")
        ;; don't put comment on trailing newline
        (setq result (substring result 0 (- (length result) (length comm)))))
    result))

(defun my-source-header ()
  "Return header string for a source file."
  (concat "Author: " (user-full-name) "\n"
          "Created: " (format-time-string "%Y-%m-%d") "\n"
          "Time-stamp: <>\n"))

(defvar copyright-organization (user-full-name)
  "Organization who holds copyright on the files with auto-inserted
licenses.")

(defun mit-license ()
  "Return string containing the text of the MIT license along with
the appropriate copyright notice."
  (concat "Copyright (C) "
          (format-time-string "%Y ") copyright-organization "\n\n"
          "Permission is hereby granted, free of charge, to any person\n"
          "obtaining a copy of this software and associated\n"
          "documentation files (the \"Software\"), to deal in the\n"
          "Software without restriction, including without limitation\n"
          "the rights to use, copy, modify, merge, publish, distribute,\n"
          "sublicense, and/or sell copies of the Software, and to\n"
          "permit persons to whom the Software is furnished to do so,\n"
          "subject to the following conditions:\n\n"
          "The above copyright notice and this permission notice shall\n"
          "be included in all copies or substantial portions of the\n"
          "Software.\n\n"
          "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY\n"
          "KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE\n"
          "WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR\n"
          "PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS\n"
          "OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR\n"
          "OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR\n"
          "OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE\n"
          "SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.\n"))

(defun insert-mit-license ()
  "Insert a copy of the MIT license at point.

Prompts for the comment type and then inserts the license with
that comment at the the beginning of every line."
  (interactive)
  (let ((comm (read-string "comment type: ")))
    (insert (prepend-comment comm (mit-license)))))

;; skeletons (aka templates) to be inserted
;; define-skeleton might be useful for cleaning this up
(setq auto-insert-alist
      '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
         nil
         '(setq v2
                (split-string (file-name-nondirectory buffer-file-name) "\\."))
         '(setq v1
                (upcase (concat (car v2) "_" (car (last v2)))))
         "/* " (my-source-header)
         '(indent-region (point-min) (point) nil)
         "*/" \n \n
    	 "#ifndef " v1 \n
         "#define " v1 \n \n _ \n \n
         "#endif // end " v1 \n)

        (("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
         nil
         "/* " (my-source-header) \n
         "Description: " \n
         '(indent-region (point-min) (point) nil)
         "*/" \n \n
         '(setq v1 (file-name-nondirectory
                    (file-name-sans-extension buffer-file-name)))
         '(setq v2 nil)
         (if (dolist (ext '(".h" ".H" ".hh" ".hpp") v2)
               (if (and (not v2) (file-exists-p (concat v1 ext)))
                   (setq v2 (concat v1 ext))))
             (concat "#include \"" v2 "\"\n\n")))

        ((makefile-bsdmake-mode . "BSD-style Makefiles")
         nil
         (prepend-comment "# " (my-source-header)) "\n")

        ((tuareg-mode . "OCaml program")
         nil
         "(* " (my-source-header)
         '(indent-region (point-min) (point) nil)
         "*)\n\n")

        ((sh-mode . "Shell script")
         nil
         "#!/bin/sh" \n \n
         (prepend-comment "# " (my-source-header)) \n
         "# Description: \n\n" )

        ((python-mode . "Python program")
         nil
         (if (y-or-n-p "Insert \"#!/usr/bin/env python\" ")
             "#!/usr/bin/env python\n\n")
         (prepend-comment "# " (my-source-header)) \n \n)

        (("\\.\\(pl\\|pm\\)\\'" . "Perl program")
         nil
         (if (y-or-n-p "Insert \"#!/usr/bin/env perl\" ")
             "#!/usr/bin/env perl\n\n")
         (prepend-comment "# " (my-source-header)) \n
         "use strict;\n\n")

        (("\\.html\\'" . "html file")
         "HTML title: "
         "<?xml version=\"1.0\"?>" \n
         "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"" \n
         "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" \n
         "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
         "xml:lang=\"en\" lang=\"en\">" \n
         "<head>" \n
         "<title>" str "</title>" \n
         "</head>" \n
         "<body>" \n \n
         "</body>" \n
         "</html>"
         '(indent-region (point-min) (point) nil))

        (("\\.el\\'" . "Emacs Lisp source")
         nil
         (prepend-comment ";; " (my-source-header)) \n
         ";;; Description: \n\n" )

        (("\\.tex\\'" . "Latex source")
         nil
         "\\documentclass{article}" \n
         "\\usepackage{fancyhdr}" \n
         "\\pagestyle{fancy}" \n
         "\\lhead{Title}" \n
         "\\chead{Paul Curry}" \n
         "\\rhead{Course}" \n
         "\\cfoot{\\thepage}" \n \n

         "\\begin{document}" \n
         _ \n
         "\\end{document}")))
