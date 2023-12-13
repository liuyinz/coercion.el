;;; coercion.el --- Naming convention style switch -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, editing
;; Homepage: https://github.com/liuyinz/coercion.el

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Inspired by coercion in abolish.vim, switch naming convention style quickly.
;; Extending features by define split and join functions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pcase)

(defgroup coercion nil
  "Naming convention styles switch."
  :group 'coercion
  :prefix "coercion-")

(defcustom coercion-enable-overlays
  '(yas--active-field-overlay)
  "Supported overlays before point to transform text.
Selected according to order."
  :type '(repeat symbol)
  :group 'coercion)

(defun coercion--bounds ()
  "Return position (START . END) of string to be handled."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (if-let ((ov (car (ignore-errors (seq-filter
                                      #'overlay-start
                                      (mapcar #'symbol-value
                                              coercion-enable-overlays))))))
        (cons (overlay-start ov) (overlay-end ov))
      (bounds-of-thing-at-point 'symbol))))

(defun coercion--split (regexp string)
  "Split STRING into a list of strings by REGEXP."
  (split-string
   (or (and (null regexp)
            (let ((case-fold-search nil))
              (replace-regexp-in-string
               "\\([[:lower:][:digit:]]\\)\\([[:upper:]]\\)" "\\1 \\2"
               (replace-regexp-in-string
                "^\\([[:upper:]]\\)\\([[:upper:]][0-9[:lower:]]\\)"
                "\\1 \\2"
                string))))
       string)
   (or regexp "[^[:word:]0-9]+") t))

(defun coercion--join (join strings)
  "Concat STRINGS into one according to function or rule JOIN."
  (mapconcat #'identity (dolist (m (butlast join) strings)
                          (setq strings (mapcar m strings)))
             (car (last join))))

(cl-defun coercion--change (&key split join)
  "Replace string selected or at point according to SPLIT and JOIN.
SPLIT must be a function or regexp to split string; nil stands for
`coercion--split'.  JOIN may be a function or a list of
\(TRANSFORM1 2 3 .. SEPARATOR).  If it's a function of one argument, must return
 a string joined from a list of strings.  If it's a list, apply TRANSFORM to
 each element of strings, and concat the results according to SEPARATOR."
  (let* ((split (or (and (functionp split) split)
                    (and (string-or-null-p split)
                         (apply-partially #'coercion--split split))))
         (join (or (and (functionp join) join)
                   (and (listp join)
                        (apply-partially #'coercion--join join)))))
    (pcase-let ((`(,beg . ,end) (coercion--bounds)))
      (if-let* ((parts (funcall split (buffer-substring-no-properties beg end)))
                ((> (length parts) 1))
                (new (funcall join parts))
                ((stringp new)))
          (progn
            (delete-region beg end)
            (goto-char beg)
            (insert new))
        (user-error "Coercion: parsing string failed!")))))

(defun coercion--join-camel-case (strings)
  "Convert a list of STRINGS to camel case string."
  (concat (downcase (car strings))
          (mapconcat 'capitalize (cdr strings))))

;;;###autoload
(defun coercion-camel-case ()
  "Convert to camel-case style."
  (interactive)
  (coercion--change :join #'coercion--join-camel-case))

;;;###autoload
(defun coercion-pascal-case ()
  "Convert to pascal-case style."
  (interactive)
  (coercion--change :join '(capitalize "")))

;;;###autoload
(defun coercion-snake-case ()
  "Convert to snake-case style."
  (interactive)
  (coercion--change :join '(downcase "_")))

;;;###autoload
(defun coercion-giraffe-case ()
  "Convert to giraffe-case style."
  (interactive)
  (coercion--change :join '(capitalize "_")))

;;;###autoload
(defun coercion-macro-case ()
  "Convert to macro-case style."
  (interactive)
  (coercion--change :join '(upcase "_")))

;;;###autoload
(defun coercion-dash-case ()
  "Convert to dash-case style."
  (interactive)
  (coercion--change :join '(downcase "-")))

;;;###autoload
(defun coercion-train-case ()
  "Convert to train-case style."
  (interactive)
  (coercion--change :join '(capitalize "-")))

;;;###autoload
(defun coercion-cobol-case ()
  "Convert to cobol-case style."
  (interactive)
  (coercion--change :join '(upcase "-")))

;;;###autoload
(defun coercion-dot-case ()
  "Convert to dot-case style."
  (interactive)
  (coercion--change :join '(downcase ".")))

;;;###autoload
(defun coercion-flat-case ()
  "Convert to flat-case style."
  (interactive)
  (coercion--change :join '(downcase "")))

;;;###autoload
(defvar-keymap coercion-command-map
  :doc "Keymap for `coercion' commands."
  :repeat t
  "c" #'coercion-camel-case
  "p" #'coercion-pascal-case
  "s" #'coercion-snake-case
  "g" #'coercion-giraffe-case
  "m" #'coercion-macro-case
  "d" #'coercion-dash-case
  "t" #'coercion-train-case
  "b" #'coercion-cobol-case
  "o" #'coercion-dot-case
  "f" #'coercion-flat-case)

(provide 'coercion)
;;; coercion.el ends here
