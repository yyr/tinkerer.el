;;; tinkerer --- Elisp wrapper for Tinkerer blogging engine.

;; Copyright (C) 2015 Yagnesh Raghava Yakkala

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; Created: 19 Feb 2015
;; Version: 0.1
;; Package-Requires: ((s))
;; Keywords: Tinkerer, blog, wrapper
;; X-URL: https://github.com/yyr/tinkerer.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

(require 's)

(defgroup tinkerer nil
  "Customizations for tinkerer wrapper."
  :group 'applications)

(defcustom tinkerer-root-path nil
  "Tinkerer blog root folder path."
  :group 'tinkerer)

(defcustom tinkerer-executable "tinker"
  "The location of the tinkerer executable."
  :group 'tinkerer
  :type 'string)

(defcustom tinkerer-keymap-prefix (kbd "C-c C-t")
  "Tinkerer keymap prefix."
  :group 'tinkerer
  :type 'string)

(defvar tinkerer-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'tinkerer-draft)
    (define-key map "D" 'tinkerer-preview-draft)
    (define-key map "P" 'tinkerer-page)
    (define-key map "p" 'tinkerer-post)
    (define-key map "b" 'tinkerer-build)))

(defvar tinkerer-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map tinkerer-keymap-prefix 'tinkerer-command-map)
    map)
  "Tinkerer key map.")

;;;###autoload
(defun tinkerer-build ()
  "Run tinkerer build command."
  (interactive)
  (let ((default-directory tinkerer-root-path))
    (async-shell-command
     (format "%s %s"
             tinkerer-executable "-b"))))

;;;###autoload
(defun tinkerer-draft (title)
  "Run tinkerer build command."
  (interactive
   (list
    (funcall #'read-from-minibuffer
             "Title of the Draft: " nil nil nil t "new-draft")))
  (let ((default-directory tinkerer-root-path))
    (find-file-other-window
     (s-trim (shell-command-to-string
              (format "%s -f %s \"%s\""
                      tinkerer-executable "-d" title))))))

;;;###autoload
(defun tinkerer-post (title)
  "Run tinkerer build command."
  (interactive
   (list
    (funcall #'read-from-minibuffer
             "Title of the  Post: " nil nil nil t "new-draft")))
  (let ((default-directory tinkerer-root-path))
    (find-file-other-window
     (s-trim (shell-command-to-string
              (format "%s -f %s \"%s\""
                      tinkerer-executable "-p" title))))))

;;;###autoload
(defun tinkerer-page (title)
  "Run tinkerer build command."
  (interactive
   (list
    (funcall #'read-from-minibuffer
             "Title of the Page: " nil nil nil t "new-draft")))
  (let ((default-directory tinkerer-root-path))
    (find-file-other-window
     (s-trim (shell-command-to-string
              (format "%s -f %s \"%s\""
                      tinkerer-executable "--page" title))))))

;;;###autoload
(defun tinkerer-preview-draft (draft)
  "Build draft preview.")

(provide 'tinkerer)
;;; tinkerer.el ends here
