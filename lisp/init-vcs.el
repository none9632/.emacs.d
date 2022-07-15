;; -*- lexical-binding: t -*-

(use-package magit
  :bind ((:map magit-status-mode-map
               ("j" . magit-next-line)
               ("k" . magit-previous-line)
               ("J" . magit-section-forward)
               ("K" . magit-section-backward)
               ("v" . evil-visual-line))
         (:map magit-log-mode-map
               ("j" . magit-next-line)
               ("k" . magit-previous-line))
         (:map magit-revision-mode-map
               ("j" . magit-next-line)
               ("k" . magit-previous-line)
               ("J" . magit-section-forward)
               ("K" . magit-section-backward))
         (:map magit-diff-mode-map
               ("j" . magit-next-line)
               ("k" . magit-previous-line)
               ("J" . magit-section-forward)
               ("K" . magit-section-backward))
         (:map transient-base-map
               ("<escape>" . transient-quit-one)))
  :init
  (leader-key-def "g" 'magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (leader-key-def
    :keymaps 'magit-status-mode-map
    "u"   'magit-section-up
    "q"   'magit-mode-bury-buffer
    "TAB" 'magit-section-show-level-2-all)
  (leader-key-def
    :keymaps 'magit-log-mode-map
    "q"   'magit-log-bury-buffer)
  (leader-key-def
    "ef" 'with-editor-finish))

(provide 'init-vcs)
