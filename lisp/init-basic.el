;; -*- lexical-binding: t -*-

(require 'init-custom)
(require 'init-funcs)

(setq user-full-name    "none"
      user-mail-address "none")

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system          'utf-8)
(setq locale-coding-system     'utf-8)

(set-language-environment      'utf-8)
(set-default-coding-systems    'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system   'utf-8)
(set-file-name-coding-system   'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-terminal-coding-system    'utf-8)
(set-selection-coding-system   'utf-8)
(modify-coding-system-alist    'process "*" 'utf-8)

(shell-command "rm -f ~/.emacs.d/session.*")
(shell-command "rm -f ~/.emacs.d/org-src-*.txt")

(defun my/set-font-faces ()
  ;; Set the font face based on platform
  (set-face-attribute 'default nil :font "SauceCodePro Nerd Font" :weight 'regular :height 150)

  ;; Set the fixed pitch fac
  (set-face-attribute 'fixed-pitch nil :font "SauceCodePro Nerd Font" :weight 'regular :height 150)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "SauceCodePro Nerd Font" :height 150 :weight 'regular))

(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                                            (with-selected-frame frame
                                              (my/set-font-faces))))
  (my/set-font-faces))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package simple
  :ensure nil
  :hook ((after-init            . size-indication-mode)
         ((prog-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode          t
        line-number-mode            t
        line-move-visual            nil
        track-eol                   t   ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode)
  :config (setq so-long-threshold 400))

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount     '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))

(setq scroll-step           1
      scroll-margin         7
      scroll-conservatively 100000)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-S-n")    'make-frame-command)
(global-set-key (kbd "M-k")      nil)

(use-package general
  :config
  (general-create-definer leader-key-def
    :keymaps '(normal
               insert
               visual
               emacs
               magit-status-mode-map
               magit-log-mode-map)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(leader-key-def
  "hk"  'helpful-key
  "hv"  'counsel-describe-variable
  "hf"  'counsel-describe-function
  "hF"  'counsel-describe-face
  "hs"  'counsel-describe-symbol
  "ch"  'counsel-command-history
  "SPC" (lambda ()
          (interactive)
          (let ((file-path (my/lf-select-file "~/Nextcloud/Projects/")))
            (unless (equal file-path "cancel")
              (find-file file-path))))
  "q"   (lambda ()
          (interactive)
          (cond ((bound-and-true-p previous-major-mode-is-org) (my/org-edit-src-exit))
                ((bound-and-true-p with-editor-mode)           (with-editor-cancel t))
                (t                                             (evil-quit))))
  "a"   'mark-whole-buffer
  "s"   'my/open-org-file
  "bb"  'counsel-switch-buffer
  "pl"  'counsel-package
  "pr"  'package-refresh-contents
  "wd"  'delete-other-windows)

(setq-default tab-width 4)
(setq indent-tabs-mode nil)

(defun disable-tabs ()
  (setq indent-tabs-mode nil))

(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t))

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default major-mode       'text-mode
              fill-column      124)

(setq visible-bell                    t
      inhibit-compacting-font-caches  t    ; Don’t compact font caches during GC.
      delete-by-moving-to-trash       t    ; Deleting files go to OS's trash folder
      make-backup-files               nil  ; Forbide to make backup files
      auto-save-default               nil  ; Disable auto save
      confirm-kill-processes          nil  ; Disable confirm killing processes on exit
      enable-local-variables          :all ; Disable confirm to set local variables
      uniquify-buffer-name-style      'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp            "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end                    "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space       nil)

(provide 'init-basic)
