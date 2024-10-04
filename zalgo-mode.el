;;; zalgo-mode.el --- Minor mode for typing Zalgo text

;; Author: Your Name
;; Version: 0.2
;; Package-Requires: ((emacs "24"))
;; Keywords: zalgo, fun
;; URL: https://your-github-url

;;; Commentary:
;; This minor mode transforms text into Zalgo text by adding
;; combining diacritical marks (accents, tilde, etc.) to make it
;; look "glitchy" or corrupted.
;;
;; To enable the mode, use:
;; M-x zalgo-mode
;;
;; You can configure the maximum number of Zalgo characters added
;; and the level of randomness.

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
  "Add Zalgo diacritical marks to CHAR."
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
    zalgo-text))

(defun zalgo-transform-text (text)
  "Transform TEXT into Zalgo text."
  (mapconcat 'zalgoify-char text ""))

(defun zalgo-after-insert (beg end &rest _)
  "Transform text inserted between BEG and END into Zalgo text."
  (when zalgo-mode
    (let ((zalgo-text (zalgo-transform-text (buffer-substring beg end))))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert zalgo-text)))))

;;;###autoload
(define-minor-mode zalgo-mode
  "Minor mode for typing Zalgo text."
  :lighter " Zalgo"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c z r") 'zalgo-transform-region)
            map)
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
