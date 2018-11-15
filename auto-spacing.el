;;; auto-spacing.el --- Insert spaces between non English terms and English terms automatically
;;
;; Copyright (C) 2018 kurubushi
;;
;; Author: kurubushi <krbshi@gmail.com>
;; Version: 0.1
;; Created: 2018-11-14
;; Licence: MIT

(defcustom auto-spacing-english-regexp (rx (in "a-zA-Z0-9"))
  "Regex pattern of English"
  :group 'auto-spacing
  :type  'regexp)

(defcustom auto-spacing-non-english-regexp (rx (category japanese))
  "Regex pattern of non English"
  :group 'auto-spacing
  :type  'regexp)

(defcustom auto-spacing-non-english-exception-regexp (rx (in "。，．！？；：「」（）、"))
  "Regex pattern of non English 2"
  :group 'auto-spacing
  :type  'regexp)

(defcustom auto-spacing-separator " "
  "Separator"
  :group 'auto-spacing
  :type  'string)


(defun auto-spacing-insert (beg end _len)
  (save-excursion
    (goto-char end)
    (if (not (bolp))
          (backward-char)
          (let ((c1 (char-to-string (preceding-char)))
                (c2 (char-to-string (following-char))))
;;            (message (concat "c1: " c1))
;;            (message (concat "c2: " c2))
            (if (or (and (string-match auto-spacing-english-regexp c1)
                         (string-match auto-spacing-non-english-regexp c2)
                         (not (string-match auto-spacing-non-english-exception-regexp c2)))
                    (and (string-match auto-spacing-non-english-regexp c1)
                         (not (string-match auto-spacing-non-english-exception-regexp c1))
                         (string-match auto-spacing-english-regexp c2)))
                (insert auto-spacing-separator))))))


(define-minor-mode auto-spacing-mode
  "Toggle auto-spacing-mode"
  :group 'auto-spacing
  :global nil
  :init-value nil
  :lighter " AS"

  ;; for skk
  (defadvice skk-insert (after skk-insert--self-insert-command)
    (self-insert-command 0))

  (if auto-spacing-mode
      (progn
        (add-hook 'post-self-insert-hook 'auto-spacing-insert)
;;        (add-hook 'after-change-functions 'auto-spacing-insert)
        (ad-activate-regexp "skk-insert--self-insert-command"))
    (progn
      (remove-hook 'post-self-insert-hook 'auto-spacing-insert)
;;      (remove-hook 'after-change-functions 'auto-spacing-insert)
      (ad-deactivate-regexp "skk-insert--self-insert-command"))))




(provide 'auto-spacing)
