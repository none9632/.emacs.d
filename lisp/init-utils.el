;; -*- lexical-binding: t -*-

(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :init (setq which-key-max-description-length 30
              which-key-show-remaining-keys t))

(use-package real-auto-save
  :hook (find-file . real-auto-save-mode)
  :config
  (setq real-auto-save-interval 1))

(use-package copyit)                    ; copy path, url, etc.
(use-package esup)                      ; Emacs startup profiler
(use-package list-environment)
(use-package memory-usage)
(use-package daemons)                   ; system services/daemons
(use-package command-log-mode)

(provide 'init-utils)
