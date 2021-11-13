;; -*- lexical-binding: t -*-

(use-package magit
  :bind (:map magit-status-mode-map
              ("j" . evil-next-line)
              ("k" . evil-previous-line))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(leader-key-def
  "g"  'magit-status
  "ef" 'with-editor-finish)

(use-package magit-todos
  :after magit
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (magit-todos-mode 1))

(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine))
  :hook (before-revert . (lambda ()
                           (when (bound-and-true-p git-timemachine-mode)
                             (user-error "Cannot revert the timemachine buffer")))))

;; (use-package gitattributes-mode)
;; (use-package gitconfig-mode)
;; (use-package gitignore-mode)

(provide 'init-vcs)
