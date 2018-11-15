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

(defcustom auto-spacing-self-insert-command-list nil
  "List of advices add self-insert-command"
  :group 'auto-spacing
  :type  '(list symbol))


(defun auto-spacing-insert ()
  (save-excursion
    (if (not (bolp))
        (progn
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
                (insert auto-spacing-separator)))))))

(defun auto-spacing-ad-activate-one (command)
  (let* ((ad-name (concat (symbol-name command) "--self-insert-command"))
         (ad (intern ad-name)))
    (if (fboundp command)
        (progn
          (eval
           (macroexpand
            `(defadvice ,command (after ,ad)
               (self-insert-command 0)))
          (ad-activate-regexp ad-name))))))

(defun auto-spacing-ad-activate ()
  (mapcar 'auto-spacing-ad-activate-one
          auto-spacing-self-insert-command-list))

(defun auto-spacing-ad-deactivate-one (command)
  (let* ((ad-name (concat (symbol-name command) "--self-insert-command")))
    (ad-deactivate-regexp ad-name)))

(defun auto-spacing-ad-deactivate ()
  (mapcar 'auto-spacing-ad-deactivate-one
          auto-spacing-self-insert-command-list))


(define-minor-mode auto-spacing-mode
  "Toggle auto-spacing-mode"
  :group 'auto-spacing
  :global nil
  :init-value nil
  :lighter " AS"

  (if auto-spacing-mode
      (progn
        (add-hook 'post-self-insert-hook 'auto-spacing-insert)
        (auto-spacing-ad-activate))
    (progn
      (remove-hook 'post-self-insert-hook 'auto-spacing-insert)
      (auto-spacing-ad-deactivate))))




(provide 'auto-spacing)
