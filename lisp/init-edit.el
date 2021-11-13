;; -*- lexical-binding: t -*-

(require 'init-evil)
(require 'init-basic)

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

(use-package avy
  :bind (:map evil-normal-state-map
         ("/" . avy-goto-char-timer)
         :map evil-visual-state-map
         ("/" . avy-goto-char-timer)
         :map evil-motion-state-map
         ("/" . avy-goto-char-timer))
  :config
  (setq avy-all-windows     nil
        avy-timeout-seconds 0.4))

(use-package ace-link
  :hook (after-init . ace-link-setup-default)
  :config
  (leader-key-def
    "bu" 'ace-link-addr))

(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode html-mode css-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line))))))

(use-package comment-dwim-2
  :init
  (leader-key-def
    "/" (lambda ()
          (interactive)
          (if (eq major-mode 'org-mode)
              (org-comment-dwim-2)
            (comment-dwim-2)))))

(use-package drag-stuff
  :diminish
  :after evil
  :hook (after-init . drag-stuff-global-mode)
  :bind ((:map evil-visual-state-map
               ("C-M-k" . drag-stuff-up)
               ("C-M-j" . drag-stuff-down))
         (:map evil-normal-state-map
               ("M-k"   . drag-stuff-up)
               ("M-j"   . drag-stuff-down))
         (:map evil-insert-state-map
               ("M-k"   . drag-stuff-up)
               ("M-j"   . drag-stuff-down)))
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode))

(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        ;; (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function       'ediff-setup-windows-plain)
  (setq ediff-split-window-function       'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package electric-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package expand-region
  :after evil
  :bind (:map evil-visual-state-map
              ("M-k" . er/expand-region)
              ("M-j" . er/contract-region)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|"           . mc/vertical-align-with-space)))

(use-package smart-region
  :hook (after-init . smart-region-on))

(use-package goto-chg
  :bind ("C-," . goto-last-change))

(use-package goto-last-point
  :diminish
  :bind ("C-M-," . goto-last-point)
  :hook (after-init . goto-last-point-mode))

(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode        . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package origami
  :hook (prog-mode . origami-mode)
  :init (setq origami-show-fold-header t)
  :config (face-spec-reset-face 'origami-fold-header-face))

(use-package sudo-edit)

(use-package prettify-symbols
  :ensure nil
  :hook (after-init . global-prettify-symbols-mode)
  :init
  (defun my/org-load-prettify-symbols ()
    (interactive)
    ;; (set-face-background 'org-block-begin-line "#282c34")
    ;; (push '("#+begin_latex latex" . ? ) prettify-symbols-alist)
    ;; (push '("#+end_latex"         . ? ) prettify-symbols-alist)
    (push '("\\\\"                . ?↵) prettify-symbols-alist))
  (defun my/latex-load-prettify-symbols ()
    (interactive)
    (push '("\\pm"        . ?±)  prettify-symbols-alist)
    (push '("\\mp"        . ?∓)  prettify-symbols-alist)
    (push '("\\left("     . ?\() prettify-symbols-alist)
    (push '("\\right)"    . ?\)) prettify-symbols-alist)
    (push '("\\left["     . ?\[) prettify-symbols-alist)
    (push '("\\right]"    . ?\]) prettify-symbols-alist)
    (push '("\\left\\{"   . ?\{) prettify-symbols-alist)
    (push '("\\right\\}"  . ?\}) prettify-symbols-alist)
    (push '("\\mathbb{N}" . ?ℕ) prettify-symbols-alist)
    (push '("\\mathbb{Z}" . ?ℤ) prettify-symbols-alist)
    (push '("\\mathbb{Q}" . ?ℚ) prettify-symbols-alist)
    (push '("\\mathbb{R}" . ?ℝ) prettify-symbols-alist)
    (push '("\\mathbb{C}" . ?ℂ) prettify-symbols-alist)
    (push '("\\not\\in"   . ?∉) prettify-symbols-alist)
    (push '("\\\\"        . ?↵) prettify-symbols-alist)))

(provide 'init-edit)
