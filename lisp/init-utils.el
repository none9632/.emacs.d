;; -*- lexical-binding: t -*-

(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :init (setq which-key-max-description-length 30
              which-key-show-remaining-keys t))

;; ;; Writable `grep' buffer
;; (use-package wgrep
;;   :init
;;   (setq wgrep-auto-save-buffer t
;;         wgrep-change-readonly-file t))

;; ;; Fast search tool `ripgrep'
;; (use-package rg
;;   :defines projectile-command-map
;;   :hook (after-init . rg-enable-default-bindings)
;;   :bind (:map rg-global-map
;;          ("c" . rg-dwim-current-dir)
;;          ("f" . rg-dwim-current-file)
;;          ("m" . rg-menu)
;;          :map rg-mode-map
;;          ("m" . rg-menu))
;;   :init (setq rg-group-result t
;;               rg-show-columns t)
;;   :config
;;   (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

;;   (with-eval-after-load 'projectile
;;     (defalias 'projectile-ripgrep #'rg-project)
;;     (bind-key "s R" #'rg-project projectile-command-map))

;;   (with-eval-after-load 'counsel
;;     (bind-keys
;;      :map rg-global-map
;;      ("R" . counsel-rg)
;;      ("F" . counsel-fzf))))

(use-package real-auto-save
  :hook (find-file . real-auto-save-mode)
  :config
  (setq real-auto-save-interval 1))

;; (use-package proced
;;   :ensure nil
;;   :init
;;   (setq-default proced-format 'verbose)
;;   (setq proced-auto-update-flag t
;;         proced-auto-update-interval 3))

(use-package copyit)                    ; copy path, url, etc.
(use-package esup)                      ; Emacs startup profiler
(use-package list-environment)
(use-package memory-usage)
(use-package daemons)                   ; system services/daemons
(use-package command-log-mode)

(provide 'init-utils)
