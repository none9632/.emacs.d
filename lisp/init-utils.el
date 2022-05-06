;; -*- lexical-binding: t -*-

(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-max-description-length 30
        which-key-show-remaining-keys    t))

(use-package real-auto-save
  :hook (find-file . real-auto-save-mode)
  :config
  (setq real-auto-save-interval 1))

(use-package posframe)
(use-package command-log-mode
  :after posframe)

(setq my/command-window-frame nil)

(defun my/toggle-command-window ()
  (interactive)
  (if my/command-window-frame
      (progn
        (posframe-delete-frame clm/command-log-buffer)
        (setq my/command-window-frame nil)
        (command-log-mode 0))
    (progn
      (command-log-mode t)
      (global-command-log-mode t)
      (with-current-buffer
          (setq clm/command-log-buffer
                (get-buffer-create " *command-log*")))
      (setq my/command-window-frame
            (posframe-show
             clm/command-log-buffer
             :position `(,(- (x-display-pixel-width) 450) . 20)
             :width 40
             :height 7
             :min-width 40
             :min-height 7
             :internal-border-width 2
             :internal-border-color "#32424b"
             :override-parameters '((parent-frame . nil)))))))

(leader-key-def
  "cl" 'my/toggle-command-window)

(use-package copyit)                    ; copy path, url, etc.
(use-package esup)                      ; Emacs startup profiler
(use-package list-environment)
(use-package memory-usage)
(use-package daemons)                   ; system services/daemons

(provide 'init-utils)
