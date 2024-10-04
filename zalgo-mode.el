;;; zalgo-mode.el --- Minor mode for typing Zalgo text  -*- lexical-binding: t -*-

;; Author: Stephen Nehrbass
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: zalgo, fun
;; Homepage: https://github.com/nehrbash/zalgo-mode


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; This minor mode transforms text into Zalgo text by adding
;; combining diacritical marks (accents, tilde, etc.) to make it
;; look glitchy or corrupted.
;;
;; To enable the mode, use:
;; M-x zalgo-mode
;;
;; You can configure the maximum number of Zalgo characters added.


;;; Code:

;; List of Zalgo combining characters
(defvar zalgo-up '("̍" "̎" "̄" "̅" "̿" "̑" "̒" "̓" "̔" "̽" "̾" "̓" "̈́" "͆" "͊" "͋" "͌" "͐" "͑" "͒" "͗" "͛" "ͣ" "ͥ" "ͨ" "ͩ" "ͪ" "ͫ" "ͬ" "ͭ" "ͮ" "ͯ"))
(defvar zalgo-mid '("̕" "̛" "̀" "́" "͘" "̡" "̢" "̧" "̨" "̴" "̵" "̶" "͜" "͝" "͞" "͟" "͠" "͢" "̸" "̷"))
(defvar zalgo-down '("̖" "̗" "̘" "̙" "̜" "̝" "̞" "̟" "̠" "̤" "̥" "̦" "̩" "̪" "̫" "̬" "̭" "̮" "̯" "̰" "̱" "̲" "̳" "̹" "̺" "̻" "̼" "ͅ" "͇" "͈" "͉" "͍" "͎" "͓" "͔" "͕" "͖"))

;; User-configurable variables
(defcustom zalgo-max-up 3
  "Maximum number of 'up' Zalgo characters added to each letter."
  :type 'integer
  :group 'zalgo)

(defcustom zalgo-max-mid 2
  "Maximum number of 'mid' Zalgo characters added to each letter."
  :type 'integer
  :group 'zalgo)

(defcustom zalgo-max-down 3
  "Maximum number of 'down' Zalgo characters added to each letter."
  :type 'integer
  :group 'zalgo)

(defun zalgo-random-element (lst)
  "Return a random element from LST."
  (elt lst (random (length lst))))

(defun zalgoify-char (char)
  "Add Zalgo diacritical marks to CHAR, skipping newline characters."
  (if (eq char ?\n)
      (char-to-string char)  ;; Return the newline character unchanged
    (let ((zalgo-text (char-to-string char)))
      ;; Random up characters
      (dotimes (_ (1+ (random zalgo-max-up)))
        (setq zalgo-text (concat zalgo-text (zalgo-random-element zalgo-up))))
      ;; Random mid characters
      (dotimes (_ (random zalgo-max-mid))
        (setq zalgo-text (concat zalgo-text (zalgo-random-element zalgo-mid))))
      ;; Random down characters
      (dotimes (_ (1+ (random zalgo-max-down)))
        (setq zalgo-text (concat zalgo-text (zalgo-random-element zalgo-down))))
      zalgo-text)))

(defun zalgo-transform-text (text)
  "Transform TEXT into Zalgo text."
  (mapconcat 'zalgoify-char text ""))

(defun zalgo-after-insert (beg end &rest _)
  "Transform text inserted between BEG and END into Zalgo text."
  (when zalgo-mode
    (let ((zalgo-text (zalgo-transform-text (buffer-substring beg end))))
      ;; Delete the original region and insert the Zalgo text
      (delete-region beg end)
      (goto-char beg)
      (insert zalgo-text)
      ;; Move cursor to the end of inserted text
      (goto-char (point)))))

(defun zalgo-transform-word ()
  "Transform the current word into Zalgo text."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (zalgo-transform-region (car bounds) (cdr bounds)))))

;;;###autoload
(define-minor-mode zalgo-mode
  "Minor mode for typing Zalgo text."
  :lighter " Zalgo"
  (if zalgo-mode
      ;; Enable the mode
      (progn
        (add-hook 'after-change-functions 'zalgo-after-insert nil t)
        (message "Zalgo mode enabled."))
    ;; Disable the mode
    (remove-hook 'after-change-functions 'zalgo-after-insert t)
    (message "Zalgo mode disabled.")))

(defun zalgo-transform-region (start end)
  "Transform the text between START and END into Zalgo text."
  (interactive "r")
  (let ((text (buffer-substring start end)))
    (delete-region start end)
    (insert (zalgo-transform-text text))))

(provide 'zalgo-mode)

;;; zalgo-mode.el ends here
