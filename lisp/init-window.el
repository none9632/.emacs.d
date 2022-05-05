;; -*- lexical-binding: t -*-

(use-package zoom
  :init
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))
   '(zoom-mode t)
   '(zoom-ignored-buffer-names '("COMMIT_EDITMSG"
                                 " *command-log*"))
   '(zoom-ignored-buffer-name-regexps '("^magit-diff"))))

(advice-add 'balance-windows :override (lambda () nil))

(use-package ace-window
  :custom-face
  :hook (emacs-startup . ace-window-display-mode)
  :config
  (defun aw--select-window (number)
    "Slecet the specified window."
    (when (numberp number)
      (let ((found nil))
        (dolist (win (aw-window-list))
          (when (and (window-live-p win)
                     (eq number
                         (string-to-number
                          (window-parameter win 'ace-window-path))))
            (setq found t)
            (aw-switch-to-window win)))
        (unless found
          (message "No specified window: %d" number)))))

  (dotimes (n 9)
    (bind-key (format "M-%d" (1+ n))
              (lambda ()
                (interactive)
                (aw--select-window (1+ n)))))

  (leader-key-def
    "wd" 'ace-delete-other-windows))

(provide 'init-window)
