;; -*- lexical-binding: t -*-

(require 'init-funcs)
(require 'init-treemacs)

(use-package ibuffer
  :after evil avy
  :ensure nil
  :bind (:map ibuffer-mode-map
         ("j" . evil-next-line)
         ("k" . evil-previous-line)
         ("h" . evil-backward-char)
         ("l" . evil-forward-char)
         ("/" . avy-goto-char-timer))
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  ;; Display icons for buffers
  (use-package all-the-icons-ibuffer
    :if (icons-displayable-p)
    :init (all-the-icons-ibuffer-mode 1))

  (with-eval-after-load 'counsel
    (with-no-warnings
      (defun my/ibuffer-find-file ()
        (interactive)
        (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                   (if (buffer-live-p buf)
                                       (with-current-buffer buf
                                         default-directory)
                                     default-directory))))
          (counsel-find-file default-directory)))
      (advice-add #'ibuffer-find-file :override #'my/ibuffer-find-file))))

(use-package ibuffer-projectile
  :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix
        (if (icons-displayable-p)
            (concat
             (all-the-icons-octicon "file-directory"
                                    :face ibuffer-filter-group-name-face
                                    :v-adjust 0.0
                                    :height 1.0)
             " ")
          "Project: ")))

(leader-key-def
  "l" (lambda ()
        (interactive)
        (if (equal major-mode 'treemacs-mode)
            (treemacs-visit-node-ace-horizontal-split)
          (progn
            (split-window-horizontally)
            (other-window 1)
            (ibuffer))))
  "j" (lambda ()
        (interactive)
        (if (equal major-mode 'treemacs-mode)
            (treemacs-visit-node-ace-vertical-split)
          (progn
            (split-window-vertically)
            (other-window 1)
            (ibuffer)))))

(provide 'init-ibuffer)
