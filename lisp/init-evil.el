;; -*- lexical-binding: t -*-

(require 'init-basic)

(use-package evil
  :hook (prog-mode . evil-mode)
  :bind ((:map evil-motion-state-map
               ("RET"     . nil)
               ("M-k"     . nil)
               ("SPC"     . nil)
               ("DEL"     . nil))
         (:map evil-normal-state-map
               ("H"       . left-word)
               ("L"       . right-word)
               ("J"       . forward-paragraph)
               ("K"       . backward-paragraph)
               ("U"       . evil-redo)
               ("SPC TAB" . evil-close-folds)
               ("S-M-h"   . evil-window-increase-width)
               ("S-M-l"   . evil-window-decrease-width)
               ("S-M-k"   . evil-window-increase-height)
               ("S-M-j"   . evil-window-decrease-height))
         (:map evil-visual-state-map
               ("H"       . left-word)
               ("L"       . right-word)
               ("J"       . forward-paragraph)
               ("K"       . backward-paragraph))
         (:map evil-insert-state-map
               ("M-k"     . nil)))
  :custom (evil-want-keybinding nil)
  :config
  (global-set-key (kbd "C-l") 'evil-window-right)
  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-k") 'evil-window-up)
  (global-set-key (kbd "C-j") 'evil-window-down)

  (define-key evil-insert-state-map [(shift backspace)] 'evil-delete-backward-word)

  (add-hook 'evil-visual-state-entry-hook (lambda () (prettify-symbols-mode -1)))
  (add-hook 'evil-visual-state-exit-hook  (lambda () (prettify-symbols-mode 1)))

  (use-package undo-fu)

  (setq evil-whan-fine-undo t
        evil-undo-system    'undo-fu
        evil-undo-function  'undo-fu-only-undo
        evil-redo-function  'undo-fu-only-redo))

(use-package evil-collection
  :after evil
  :config
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
