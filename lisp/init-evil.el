;; -*- lexical-binding: t -*-

(require 'init-basic)

(use-package evil
  :hook (prog-mode . evil-mode)
  :bind ((:map evil-motion-state-map
               ("RET" . nil)
               ("M-k" . nil)
               ("SPC" . nil)
               ("DEL" . nil))
         (:map evil-normal-state-map
               ("H"   . left-word)
               ("L"   . right-word)
               ("J"   . evil-forward-paragraph)
               ("K"   . evil-backward-paragraph)
               ("U"   . evil-redo)
               ("M-h" . evil-window-increase-width)
               ("M-l" . evil-window-decrease-width)
               ("M-k" . evil-window-increase-height)
               ("M-j" . evil-window-decrease-height)
               ("C-p" . my/paste-from-clipboard)
               ("y"   . my/copy-to-clipboard)
               ("p"   . (lambda ()
                          (interactive)
                          (evil-paste-after 1)
                          (if (eq major-mode 'org-mode)
                              (let ((end-point   (mark t))
                                    (start-point (point)))
                                (org-display-inline-images nil t start-point end-point)
                                (org--latex-preview-region start-point end-point))))))
         (:map evil-visual-state-map
               ("H"   . left-word)
               ("L"   . right-word)
               ("J"   . evil-forward-paragraph)
               ("K"   . evil-backward-paragraph)
               ("p"   . my/paste-from-clipboard)
               ("y"   . my/copy-to-clipboard))
         (:map evil-insert-state-map
               ("M-k" . nil)
               ("C-p" . my/paste-from-clipboard)))
  :custom (evil-want-keybinding nil)
  :config
  (global-set-key (kbd "C-l") 'evil-window-right)
  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-k") 'evil-window-up)
  (global-set-key (kbd "C-j") 'evil-window-down)

  (define-key evil-insert-state-map [(shift backspace)] 'evil-delete-backward-word)

  (add-hook 'evil-visual-state-entry-hook (lambda () (prettify-symbols-mode -1)))
  (add-hook 'evil-visual-state-exit-hook  (lambda () (prettify-symbols-mode 1)))

  (evil-define-key '(normal insert visual)
    dashboard-mode-map (kbd "r") 'dashboard-jump-to-recents)
  (evil-define-key '(normal insert visual)
    dashboard-mode-map (kbd "p") 'dashboard-jump-to-projects)

  (use-package undo-fu
    :custom
    (evil-want-fine-undo  t)
    (evil-undo-system     'undo-fu)
    (evil-undo-function   'undo-fu-only-undo)
    (evil-redo-function   'undo-fu-only-redo))

  (defun my/paste-from-clipboard ()
    (interactive)
    (if (eq evil-visual-state-minor-mode t)
        (kill-region (region-beginning) (region-end)))
    (x-clipboard-yank)
    (goto-char (mark t)))

  (defun my/copy-to-clipboard ()
    (interactive)
    (setq select-enable-clipboard t)
    (call-interactively 'evil-yank)
    (setq select-enable-clipboard nil)))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package key-chord
  :after evil
  :hook (prog-mode . key-chord-mode)
  :config
  (setq key-chord-two-keys-delay 0.15)
  (key-chord-define evil-insert-state-map (kbd "jj") 'evil-normal-state)

  (define-key evil-insert-state-map "о" #'my/maybe-exit)

  (evil-define-command my/maybe-exit ()
    :repeat change
    (interactive)
    (insert "о")
    (let ((evt (read-event "" nil 0.18)))
      (cond
       ((null evt))
       ((and (integerp evt) (char-equal evt ?о))
        (delete-char -1)
        (evil-normal-state))
       (t (insert evt))))))

(provide 'init-evil)
