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

(use-package page-break-lines
  :ensure t
  :demand t)

(use-package dashboard
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  ;; :bind (("<f2>" . open-dashboard)
  ;;        :map dashboard-mode-map
  ;;        ("R"    . restore-previous-session)
  ;;        ("L"    . restore-session)
  ;;        ("U"    . update-config-and-packages)
  ;;        ("q"    . quit-dashboard))
  :bind (:map dashboard-mode-map
              ([remap dashboard-next-line]     . widget-forward)
              ([remap dashboard-previous-line] . widget-backward))
  :init
  (defun my/dashboard-banner ()
    (defvar package-count 0)
    (when (bound-and-true-p package-alist)
      (setq package-count (length package-activated-list)))
    (when (boundp 'straight--profile-cache)
      (setq package-count (+ (hash-table-size straight--profile-cache) package-count)))
    (setq dashboard-init-info
          (format "%d packages loaded in %.3f seconds\n"
                  package-count
                  (float-time (time-subtract after-init-time before-init-time)))))

  (setq dashboard-startup-banner    'logo
        dashboard-page-separator    "\n\f\n"
        dashboard-center-content    t
        dashboard-show-shortcuts    nil
        dashboard-set-footer        nil
        dashboard-set-init-info     t
        dashboard-set-file-icons    t
        dashboard-set-heading-icons t
        dashboard-set-navigator     nil
        dashboard-projects-backend  'projectile
        dashboard-items             '((projects  . 10)
                                      (recents   . 8)))

  (add-hook 'after-init-hook                 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook             'my/dashboard-banner)
  (add-hook 'dashboard-after-initialize-hook (lambda ()
                                               (if (or (not (eq dashboard-projects-alist nil))
                                                       (not (eq dashboard-recentf-alist  nil)))
                                                   (widget-forward 1))))
  (add-hook 'dashboard-mode-hook             (lambda ()
                                               (with-current-buffer "*dashboard*"
                                                 (setq-local evil-normal-state-cursor '(nil)
                                                             doom-modeline-height     34))
                                               (if (or (not (eq dashboard-projects-alist nil))
                                                       (not (eq dashboard-recentf-alist  nil)))
                                                   (widget-forward 1))
                                               (hl-line-mode t)))
  (dashboard-setup-startup-hook)
  ;; :config
  ;; (defvar dashboard-recover-layout-p nil
  ;; 	"Wether recovers the layout.")

  ;; (defun open-dashboard ()
  ;; 	"Open the *dashboard* buffer and jump to the first widget."
  ;; 	(interactive)
  ;; 	;; Check if need to recover layout
  ;; 	(if (> (length (window-list-1))
  ;;          ;; exclude `treemacs' window
  ;;          (if (and (fboundp 'treemacs-current-visibility)
  ;; 					(eq (treemacs-current-visibility) 'visible))
  ;;              2
  ;; 			 1))
  ;; 		(setq dashboard-recover-layout-p t))
  ;; 	(delete-other-windows)
  ;; 	;; Refresh dashboard buffer
  ;; 	(when (get-buffer dashboard-buffer-name)
  ;;     (kill-buffer dashboard-buffer-name))
  ;; 	(dashboard-insert-startupify-lists)
  ;; 	(switch-to-buffer dashboard-buffer-name)
  ;; 	;; Jump to the first section
  ;; 	(dashboard-jump-to-recent-files))

  ;; (defun restore-previous-session ()
  ;; 	"Restore the previous session."
  ;; 	(interactive)
  ;; 	(when (bound-and-true-p persp-mode)
  ;;     (restore-session persp-auto-save-fname)))

  ;; (defun restore-session (fname)
  ;; 	"Restore the specified session."
  ;; 	(interactive (list (read-file-name "Load perspectives from a file: "
  ;;                                      persp-save-dir)))
  ;; 	(when (bound-and-true-p persp-mode)
  ;;     (message "Restoring session...")
  ;;     (quit-window t)
  ;;     (condition-case-unless-debug err
  ;;         (persp-load-state-from-file fname)
  ;; 		(error "Error: Unable to restore session -- %s" err))
  ;;     (message "Restoring session...done")))

  ;; (defun quit-dashboard ()
  ;; 	"Quit dashboard window."
  ;; 	(interactive)
  ;; 	(quit-window t)
  ;; 	(when (and dashboard-recover-layout-p
  ;;              (bound-and-true-p winner-mode))
  ;;     (winner-undo)
  ;;     (setq dashboard-recover-layout-p nil)))
  )

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
           completion-in-region-mode) . hide-mode-line-mode)))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package all-the-icons
  :if (display-graphic-p)
  :init (unless (find-font (font-spec :name "all-the-icons"))
          (all-the-icons-install-fonts t)))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode)
         (conf-mode . display-line-numbers-mode)
         (yaml-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode)
         (org-mode  . (lambda ()
                        (display-line-numbers-mode 0)))))

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
