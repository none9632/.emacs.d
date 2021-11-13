;; -*- lexical-binding: t -*-

(use-package elisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . (lambda ()
                             "Disable the checkdoc checker."
                             (setq-local flycheck-disabled-checkers
                                         '(emacs-lisp-checkdoc))))
  :config
  (when (boundp 'elisp-flymake-byte-compile-load-path)
    (add-to-list 'elisp-flymake-byte-compile-load-path load-path))

  ;; Syntax highlighting of known Elisp symbols
  (use-package highlight-defined
    :hook (emacs-lisp-mode . highlight-defined-mode)
    :init
    (setq highlight-defined-face-use-itself t)))

(use-package helpful
  :defines (counsel-describe-function-function
            counsel-describe-variable-function)
  :commands helpful--buffer
  :bind (([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         ("C-c C-d" . helpful-at-point)
         :map helpful-mode-map
         ("r" . remove-hook-at-point))
  :hook (helpful-mode . cursor-sensor-mode) ; for remove-advice button
  :init
  (with-eval-after-load 'counsel
    (setq counsel-describe-function-function #'helpful-callable
          counsel-describe-variable-function #'helpful-variable))

  (with-eval-after-load 'apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol))))))

  ;; Add remove buttons for advices
  ;; (define-advice helpful-update (:after () advice-remove-button)
  ;;   (when helpful--callable-p
  ;;     (add-button-to-remove-advice (helpful--buffer helpful--sym t) helpful--sym)))
  :config
  (with-no-warnings
    ;; Open the buffer in other window
    (defun my/helpful--navigate (button)
      "Navigate to the path this BUTTON represents."
      (find-file-other-window (substring-no-properties (button-get button 'path)))
      ;; We use `get-text-property' to work around an Emacs 25 bug:
      ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=f7c4bad17d83297ee9a1b57552b1944020f23aea
      (-when-let (pos (get-text-property button 'position
                                         (marker-buffer button)))
        (helpful--goto-char-widen pos)))
    (advice-add #'helpful--navigate :override #'my/helpful--navigate)))

(provide 'init-elisp)
