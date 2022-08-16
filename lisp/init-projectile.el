;; -*- lexical-binding: t -*-

(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix    ""
        projectile-sort-order          'recentf
        projectile-use-git-grep        t
        projectile-enable-caching      t
        projectile-project-search-path '("~/Cloud/Projects"
                                         "~/Nextcloud/Projects"
                                         "~/Projects"))
  :config
  (projectile-update-mode-line)         ; Update mode-line at the first time

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd)))))

(provide 'init-projectile)
