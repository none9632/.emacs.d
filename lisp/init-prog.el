;; -*- lexical-binding: t -*-

(require 'init-custom)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("C-M-j" . dumb-jump-hydra/body))
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector        'ivy))

(use-package css-mode
  :ensure nil
  :config
  (setq css-fontify-colors nil))

(use-package makefile-mode
  :ensure nil
  :hook (makefile-mode . enable-tabs))

(use-package lua-mode
  :after evil
  :config
  (evil-define-key 'normal lua-mode-map (kbd "K") nil))

(use-package yaml-mode)
(use-package vimrc-mode)
(use-package fish-mode)

(provide 'init-prog)
