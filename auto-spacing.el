;;; auto-spacing.el --- Insert spaces between non English terms and English terms automatically

;; Copyright (C) 2018-2022 kurubushi
;;
;; Author: kurubushi <krbshi@gmail.com>
;; Version: 0.1.1
;; Created: 2018-11-14
;; Licence: MIT

;;; Commentary:

;; `auto-spacing' provides `auto-spacing-mode' and `global-auto-spacing-mode'
;; which inserts spaces between non-English terms and English terms.

;;; Code:

(defcustom auto-spacing-english-regexp (rx (in "a-zA-Z0-9"))
  "Regex pattern of English."
  :group 'auto-spacing
  :type  'regexp)

(defcustom auto-spacing-non-english-regexp (rx (category japanese))
  "Regex pattern of non English."
  :group 'auto-spacing
  :type  'regexp)

(defcustom auto-spacing-non-english-exception-regexp (rx (in "。，．！？；：「」（）、"))
  "Regex pattern of non-English but without spaces."
  :group 'auto-spacing
  :type  'regexp)

(defcustom auto-spacing-separator " "
  "Separator."
  :group 'auto-spacing
  :type  'string)

(defcustom auto-spacing-self-insert-command-list '(skk-insert)
  "List of advices add `self-insert-command'."
  :group 'auto-spacing
  :type  '(list symbol))

(defun auto-spacing-insert ()
  "Insert spaces automatically."
  (save-excursion
    (if (not (bolp))
        (progn
          (backward-char)
          (let ((c1 (char-to-string (preceding-char)))
                (c2 (char-to-string (following-char))))
            (if (or (and (string-match auto-spacing-english-regexp c1)
                         (string-match auto-spacing-non-english-regexp c2)
                         (not (string-match auto-spacing-non-english-exception-regexp c2)))
                    (and (string-match auto-spacing-non-english-regexp c1)
                         (not (string-match auto-spacing-non-english-exception-regexp c1))
                         (string-match auto-spacing-english-regexp c2)))
                (insert auto-spacing-separator)))))))

(defun auto-spacing-update-advice-one (command)
  "Define advice for COMMAND to insert spaces."
  (let* ((ad-name (concat (symbol-name command) "--self-insert-command"))
         (ad (intern ad-name)))
        (progn
          (eval
           (macroexpand
            `(defadvice ,command (after ,ad)
               (self-insert-command 0)))))))

(defun auto-spacing-update-advice ()
  "Define advices for `auto-spacing-self-insert-command-list'."
  (mapcar 'auto-spacing-update-advice-one
          auto-spacing-self-insert-command-list))

(defun auto-spacing-ad-activate-one (command)
  "Activate advice for COMMAND to insert spaces."
  (let* ((ad-name (concat (symbol-name command) "--self-insert-command")))
    (ad-activate-regexp ad-name)))

(defun auto-spacing-ad-activate ()
  "Activate advices for `auto-spacing-self-insert-command-list' to insert spaces."
  (mapcar 'auto-spacing-ad-activate-one
          auto-spacing-self-insert-command-list))

(defun auto-spacing-ad-deactivate-one (command)
  "Deactivate advice for COMMAND to stop inserting spaces."
  (let* ((ad-name (concat (symbol-name command) "--self-insert-command")))
    (ad-deactivate-regexp ad-name)))

(defun auto-spacing-ad-deactivate ()
  "Deactivate advices for `auto-spacing-self-insert-command-list' to stop inserting spaces."
  (mapcar 'auto-spacing-ad-deactivate-one
          auto-spacing-self-insert-command-list))

;;; mode

(define-minor-mode auto-spacing-mode
  "Toggle auto-spacing-mode."
  :group 'auto-spacing
  :global nil
  :init-value nil
  :lighter " AS"

  (auto-spacing-update-advice)
  (if auto-spacing-mode
      (progn
        (add-hook 'post-self-insert-hook 'auto-spacing-insert nil t)
        (auto-spacing-ad-activate))
    (progn
      (remove-hook 'post-self-insert-hook 'auto-spacing-insert t)
      (auto-spacing-ad-deactivate))))

(define-globalized-minor-mode global-auto-spacing-mode auto-spacing-mode
  (lambda () (auto-spacing-mode 1)))

(provide 'auto-spacing)

;;; auto-spacing.el ends here
