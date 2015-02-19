;;; tinkerer.el --- Elisp wrapper for Tinkerer Blogging Engine.

;; Copyright (C) 2015 Yagnesh Raghava Yakkala

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; Created: 19 Feb 2015
;; Version: 0.1
;; Package-Requires: ((s "0"))
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

;;; Code:

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

;;;
(defvar tinkerer--hist nil)

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
  "Read TITLE and create a draft with it."
  (interactive
   (list
    (funcall #'read-from-minibuffer
             "Title of the Draft: " nil nil nil tinkerer--hist "new-draft")))
  (let ((default-directory tinkerer-root-path))
    (find-file-other-window
     (s-trim (shell-command-to-string
              (format "%s -f %s \"%s\""
                      tinkerer-executable "-d" title))))))

;;;###autoload
(defun tinkerer-post (arg &optional title)
  "Read TITLE and create a post with it.
If prefix argument ARG provided move a draft and publish it."
  (interactive "p")
  (if (not (= arg 4))
      ;; new post
      (let ((title (if title
                       title
                     (funcall #'read-from-minibuffer
                              "Title of the  Post: " nil nil nil tinkerer--hist "new-draft")))
            (default-directory tinkerer-root-path))
        (find-file-other-window
         (s-trim (shell-command-to-string
                  (format "%s -f %s \"%s\""
                          tinkerer-executable "-p" title)))))
    ;; move draft and post.
    (let* ((drafts-path (expand-file-name "drafts" tinkerer-root-path))
           (draft (ido-completing-read "Draft file: " (directory-files drafts-path)
                                       nil t nil tinkerer--hist)))
      (let ((default-directory tinkerer-root-path)
            (temp-buf "*temp-buf*"))
        (async-shell-command
         (format "%s %s %s %s"
                 tinkerer-executable "-f"
                 "-p" (expand-file-name draft drafts-path)))))))

;;;###autoload
(defun tinkerer-page (title)
  "Read TITLE and create a page with it."
  (interactive
   (list
    (read-from-minibuffer
     "Title of the Page: "  nil nil nil tinkerer--hist "new-draft")))
  (let ((default-directory tinkerer-root-path))
    (find-file-other-window
     (s-trim (shell-command-to-string
              (format "%s -f %s \"%s\""
                      tinkerer-executable "--page" title))))))

;;;###autoload
(defun tinkerer-preview-draft ()
  "Build draft preview."
  (interactive)
  (let* ((drafts-path (expand-file-name "drafts" tinkerer-root-path))
         (draft (ido-completing-read "Draft file: " (directory-files drafts-path)
                                     nil t nil tinkerer--hist)))
    (let ((default-directory tinkerer-root-path))
      (async-shell-command
       (format "%s %s %s"
               tinkerer-executable "--preview"
               (expand-file-name draft drafts-path))))))

(provide 'tinkerer)
;;; tinkerer.el ends here
