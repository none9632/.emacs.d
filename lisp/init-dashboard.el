;; -*- lexical-binding: t -*-

(require 'all-the-icons)
(require 'init-custom)

(use-package dashboard
  :diminish (dashboard-mode)
  :functions (all-the-icons-faicon
              all-the-icons-material
              widget-forward)
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("R"    . restore-previous-session)
         ("L"    . restore-session)
         ("U"    . update-config-and-packages)
         ("q"    . quit-dashboard))
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
  (add-hook 'after-init-hook                 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook             'my/dashboard-banner)
  (add-hook 'dashboard-after-initialize-hook 'dashboard-jump-to-recent-files)

  (setq dashboard-startup-banner    'logo
        dashboard-center-content    t
        dashboard-show-shortcuts    nil
        dashboard-set-footer        nil
        dashboard-set-init-info     t
        dashboard-set-file-icons    t
        dashboard-set-heading-icons t
        dashboard-set-navigator     nil
        dashboard-page-separator    "\n\f\n"
        dashboard-items             '((recents   . 8)
                                      (projects  . 5)
                                      (bookmarks . 5))
        dashboard-heading-icons     '((recents   . "file-text")
                                      (bookmarks . "bookmark")
                                      (projects  . "briefcase")))

  (dashboard-setup-startup-hook)
  :config
  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

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
    (dashboard-jump-to-recent-files))

  (defun restore-previous-session ()
    "Restore the previous session."
    (interactive)
    (when (bound-and-true-p persp-mode)
      (restore-session persp-auto-save-fname)))

  (defun restore-session (fname)
    "Restore the specified session."
    (interactive (list (read-file-name "Load perspectives from a file: "
                                       persp-save-dir)))
    (when (bound-and-true-p persp-mode)
      (message "Restoring session...")
      (quit-window t)
      (condition-case-unless-debug err
          (persp-load-state-from-file fname)
        (error "Error: Unable to restore session -- %s" err))
      (message "Restoring session...done")))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when (and dashboard-recover-layout-p
               (bound-and-true-p winner-mode))
      (winner-undo)
      (setq dashboard-recover-layout-p nil))))

(provide 'init-dashboard)
