;; -*- lexical-binding: t -*-

(require 'init-custom)

(use-package dashboard
  :diminish (dashboard-mode)
  :functions (all-the-icons-faicon
              all-the-icons-material
              winner-undo
              widget-forward)
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("R"    . restore-previous-session)
         ("L"    . restore-session)
         ("S"    . open-org-file)
         ("U"    . update-config-and-packages)
         ("q"    . quit-dashboard)
         ("h"    . dashboard-hydra/body)
         ("?"    . dashboard-hydra/body))
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
  :preface
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
  :init
  (add-hook 'after-init-hook     'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'my/dashboard-banner)

  (setq dashboard-startup-banner    'logo
        dashboard-center-content    t
        dashboard-show-shortcuts    nil
        dashboard-set-footer        t
        dashboard-set-init-info     t
        dashboard-set-file-icons    t
        dashboard-page-separator    "\n\f\n"
        dashboard-set-heading-icons t
        dashboard-items             '((recents   . 6)
                                      (bookmarks . 5)
                                      (projects  . 5))
        dashboard-heading-icons     '((recents   . "file-text")
                                      (bookmarks . "bookmark")
                                      (projects  . "briefcase"))

        dashboard-set-navigator t
        dashboard-navigator-buttons
        `(((,(when (icons-displayable-p)
               (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
            "Restore" "Restore previous session"
            (lambda (&rest _)))
           (,(when (icons-displayable-p)
               (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
            "Settings" "Open org file"
            (lambda (&rest _) ))
           (,(when (icons-displayable-p)
               (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
            "Update" "Update Emacs"
            (lambda (&rest _) (update-packages)))
           (,(if (icons-displayable-p)
                 (all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
               "?")
            "" "Help (?/h)"
            (lambda (&rest _) (dashboard-hydra/body))
            font-lock-string-face))))

  (dashboard-setup-startup-hook)
  :config
  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (let ((func (local-key-binding "r")))
      (and func (funcall func))))

  (defun dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (let ((func (local-key-binding "p")))
      (and func (funcall func))))

  (defun dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (let ((func (local-key-binding "m")))
      (and func (funcall func))))

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; Check if need to recover layout
    (if (> (length (window-list-1))
           ;; exclude `treemacs' window
           (if (and (fboundp 'treemacs-current-visibility)
                    (eq (treemacs-current-visibility) 'visible))
               2
             1))
        (setq dashboard-recover-layout-p t))

    (delete-other-windows)

    ;; Refresh dashboard buffer
    (when (get-buffer dashboard-buffer-name)
      (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)

    ;; Jump to the first section
    (dashboard-goto-recent-files))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when (and dashboard-recover-layout-p
               (bound-and-true-p winner-mode))
      (winner-undo)
      (setq dashboard-recover-layout-p nil))))

(provide 'init-dashboard)
