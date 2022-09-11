;;; coercion.el --- Naming convention style switch -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: wp, text, editing
;; Homepage: https://github.com/liuyinz/emacs-coercion

;; This file is not a part of GNU Emacsl.

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

(defgroup coercion nil
  "Naming convention styles switch."
  :group 'coercion
  :prefix "coercion-")

(defvar coercion--commands
  '((:cmd coercion-snake-case   :split nil :join '(downcase    "_"))
    (:cmd coercion-giraffe-case :split nil :join '(capitalize  "_"))
    (:cmd coercion-macro-case   :split nil :join '(upcase      "_"))
    (:cmd coercion-dash-case    :split nil :join '(downcase    "-"))
    (:cmd coercion-train-case   :split nil :join '(capitalize  "-"))
    (:cmd coercion-cobol-case   :split nil :join '(upcase      "-"))
    (:cmd coercion-dot-case     :split nil :join '(downcase    "."))
    (:cmd coercion-flat-case    :split nil :join '(downcase    ""))
    (:cmd coercion-pascal-case  :split nil :join '(capitalize  ""))
    (:cmd coercion-camel-case
     :split nil
     :join (lambda (s) (concat (downcase (car s)) (mapconcat 'capitalize (cdr s))))))
  "Default convention styles in coercion.")

(defun coercion--split (regexp string)
  "Split STRING into a list of substrings by REGEXP."
  (split-string
   (or (and (null regexp)
            (let ((case-fold-search nil))
              (replace-regexp-in-string
               "\\([[:lower:][:digit:]]\\)\\([[:upper:]]\\)" "\\1 \\2"
               (replace-regexp-in-string
                "^\\([[:upper:]]\\)\\([[:upper:]][0-9[:lower:]]\\)" "\\1 \\2" string))))
       string)
   (or regexp "[^[:word:]0-9]+") t))

(defun coercion--join (join substrings)
  "Concat SUBSTRINGS into one according to function or rule JOIN."
  (mapconcat #'identity (dolist (m (butlast join) substrings)
                          (setq substrings (mapcar m substrings)))
             (car (last join))))

(defun coercion--change (split join)
  "Replace string selected or at point according to function SPLIT and JOIN."
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (begin (car bounds))
         (end (cdr bounds))
         (split (or (and (functionp split) split)
                    (and (string-or-null-p split) (apply-partially #'coercion--split split))))
         (join (or (and (functionp join) join)
                   (and (listp join) (apply-partially #'coercion--join join))))
         (substrings (funcall split (buffer-substring-no-properties begin end))))
    (if (> (length substrings) 1)
        (replace-region-contents begin end (lambda () (funcall join substrings)))
      (message "Coercion: spliting string failed!"))))

(defmacro coercion-define (cmd split join)
  "Define coercion command CMD with function SPLIT and JOIN.
SPLIT must be a function or regexp to split string; nil stands for
`coercion--split'.  JOIN may be a function or a list of
\(TRANSFORM1 2 3 .. SEPARATOR).  If it's a function of one argument, must return
 a string concated by a list of substrings.  If it's a list, apply TRANSFORM to
 each element of strings, and concat the results according to SEPARATOR."
  `(defun ,cmd ()
     (interactive)
     (coercion--change ,split ,join)))

(dolist (item coercion--commands)
  (let ((cmd (plist-get item :cmd))
        (split (plist-get item :split))
        (join (plist-get item :join)))
    (eval `(coercion-define ,cmd ,split ,join))))

(provide 'coercion)
;;; coercion.el ends here
