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
  :after evil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode))
  :bind (:map evil-normal-state-map
              ("C-<return>" . goto-address-at-point)))

(use-package avy
  :after evil
  :bind (:map evil-normal-state-map
              ("/" . avy-goto-char-timer)
              :map evil-visual-state-map
              ("/" . avy-goto-char-timer)
              :map evil-motion-state-map
              ("/" . avy-goto-char-timer))
  :config
  (setq avy-all-windows     nil
        avy-timeout-seconds 0.4))

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

  ;; Be slightly less aggressive in C/C++/Java/lua
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

(use-package electric-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package expand-region
  :after evil
  :bind (:map evil-visual-state-map
              ("<tab>"     . er/expand-region)
              ("<backtab>" . er/contract-region)))

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

(use-package goto-chg
  :init
  (leader-key-def
    ";" 'goto-last-change
    "," 'goto-last-change-reverse))

(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode        . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package prettify-symbols
  :ensure nil
  :hook (after-init . global-prettify-symbols-mode)
  :init
  (defun my/org-load-prettify-symbols ()
    (interactive)
    (set-face-background 'org-block-begin-line "#282c34")
    (push '("#+begin_latex latex" . ? ) prettify-symbols-alist)
    (push '("#+end_latex"         . ? ) prettify-symbols-alist)
    (push '("\\\\"                . ?↵) prettify-symbols-alist))
  (defun my/latex-load-prettify-symbols ()
    (interactive)
    (push '("\\pm"           . ?±)  prettify-symbols-alist)
    (push '("\\mp"           . ?∓)  prettify-symbols-alist)
    (push '("\\left("        . ?\() prettify-symbols-alist)
    (push '("\\right)"       . ?\)) prettify-symbols-alist)
    (push '("\\left["        . ?\[) prettify-symbols-alist)
    (push '("\\right]"       . ?\]) prettify-symbols-alist)
    (push '("\\left\\{"      . ?\{) prettify-symbols-alist)
    (push '("\\right\\}"     . ?\}) prettify-symbols-alist)
    (push '("\\left|"        . ?|)  prettify-symbols-alist)
    (push '("\\right|"       . ?|)  prettify-symbols-alist)
    (push '("\\Bigl("        . ?\() prettify-symbols-alist)
    (push '("\\Bigr)"        . ?\)) prettify-symbols-alist)
    (push '("\\Bigl["        . ?\[) prettify-symbols-alist)
    (push '("\\Bigr]"        . ?\]) prettify-symbols-alist)
    (push '("\\Bigl\\{"      . ?\{) prettify-symbols-alist)
    (push '("\\Bigr\\}"      . ?\}) prettify-symbols-alist)
    (push '("\\N"            . ?ℕ) prettify-symbols-alist)
    (push '("\\Z"            . ?ℤ) prettify-symbols-alist)
    (push '("\\Q"            . ?ℚ) prettify-symbols-alist)
    (push '("\\R"            . ?ℝ) prettify-symbols-alist)
    (push '("\\C"            . ?ℂ) prettify-symbols-alist)
    (push '("\\not\\in"      . ?∉) prettify-symbols-alist)
    (push '("\\uua"          . ?⇈) prettify-symbols-alist)
    (push '("\\uda"          . ?⇅) prettify-symbols-alist)
    (push '("\\Lra"          . ?⇔) prettify-symbols-alist)
    (push '("\\La"           . ?⇐) prettify-symbols-alist)
    (push '("\\Ra"           . ?⇒) prettify-symbols-alist)
    (push '("\\degree"       . ?⚬) prettify-symbols-alist)
    (push '("\\triangle"     . ?Δ) prettify-symbols-alist)
    (push '("\\vepsilon"     . ?ε) prettify-symbols-alist)
    (push '("\\Vepsilon"     . ?Ɛ) prettify-symbols-alist)
    (push '("\\vphi"         . ?φ) prettify-symbols-alist)
    (push '("\\\\"           . ?↵) prettify-symbols-alist)))

(provide 'init-edit)
