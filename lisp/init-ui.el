;; -*- lexical-binding: t -*-

(require 'init-custom)
(require 'init-funcs)

(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise       t)

;; Make certain buffers grossly incandescent
;; Must before loading the theme
(use-package solaire-mode
  :hook ((minibuffer-setup        . solaire-mode-fix-minibuffer)
         (server-after-make-frame . solaire-mode-fix-minibuffer))
  :custom
  (solaire-global-mode +1))

(use-package doom-themes
  :after solaire-mode
  :hook (server-after-make-frame . (lambda ()
                                     (load-theme 'doom-one t)))
  :custom-face (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
  :custom (doom-themes-treemacs-theme "doom-colors")
  :init
  (unless (daemonp)
    (load-theme 'doom-one t))
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable customized theme
  (doom-themes-treemacs-config))

(use-package doom-modeline
  :custom
  (doom-modeline-icon             t)
  (doom-modeline-minor-modes      t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-mu4e             nil)
  (doom-modeline-height           34)
  :hook (after-init . doom-modeline-mode)
  :init
  ;; Prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq doom-modeline--default-format mode-line-format)
    (setq-default mode-line-format nil)))

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           flycheck-error-list-mode) . hide-mode-line-mode)))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package all-the-icons
  :if (display-graphic-p)
  :init (unless (find-font (font-spec :name "all-the-icons"))
          (all-the-icons-install-fonts t)))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

(setq use-file-dialog                   nil
      use-dialog-box                    nil
      inhibit-startup-screen            t
      inhibit-startup-echo-area-message t)

(setq window-divider-default-places       t
      window-divider-default-bottom-width 1
      window-divider-default-right-width  1)

(add-hook 'window-setup-hook #'window-divider-mode)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") (lambda () (text-scale-mode 0)))

(provide 'init-ui)
