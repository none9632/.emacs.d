;; -*- lexical-binding: t -*-

(require 'init-custom)
(require 'init-evil)
(require 'init-vcs)

(use-package treemacs
  :after evil
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :bind (:map treemacs-mode-map
              ("d"   . treemacs-delete)
              ("r"   . treemacs-rename)
              ("C-n" . treemacs)
              ("C-/" . treemacs-helpful-hydra)
              ("C-j" . evil-window-down)
              :map evil-normal-state-map
              ("C-n" . my/treemacs-select-window)
              :map evil-visual-state-map
              ("C-n" . my/treemacs-select-window)
              :map evil-insert-state-map
              ("C-n" . my/treemacs-select-window))
  :custom-face (treemacs-git-modified-face ((t (:foreground "#51afef"))))
  :config
  (setq treemacs-collapse-dirs     0
        treemacs-sorting           'alphabetic-asc
        treemacs-follow-after-init t
        treemacs-width             30
        treemacs-file-event-delay  500)

  (defun my/treemacs-select-window ()
    (interactive)
    (if (equal major-mode 'treemacs-mode)
        (treemacs)
      (treemacs-select-window)))

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)

  (add-hook 'treemacs-mode-hook (lambda ()
                                  (setq-local evil-normal-state-cursor '(nil))
                                  (set (make-variable-buffer-local 'scroll-margin) 0))))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile
  :bind (:map projectile-command-map
         ("h" . treemacs-projectile)))

(use-package treemacs-magit
  :after treemacs magit
  :commands treemacs-magit--schedule-update
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage)
         . treemacs-magit--schedule-update))

(provide 'init-treemacs)
