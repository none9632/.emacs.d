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
  (defvar my/exclude-electic-pair-modes '(latex-mode))

  (defun my/inhibit-electric-pair-mode (char)
    (member major-mode my/exclude-electic-pair-modes))

  (setq electric-pair-inhibit-predicate #'my/inhibit-electric-pair-mode))

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
  :after evil
  :bind (:map evil-normal-state-map
              (";" . goto-last-change)
              ("," . goto-last-change-reverse)))

(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode        . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package hideshow
  :after evil
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (:map evil-normal-state-map
              ("TAB" . hs-cycle))
  :config
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))

(use-package prettify-symbols
  :ensure nil
  :hook (after-init . global-prettify-symbols-mode)
  :init
  (defun my/org-load-prettify-symbols ()
    (interactive)
    (set-face-background 'org-block-begin-line "#282c34")
    (push '("#+begin_latex latex" . ? ) prettify-symbols-alist)
    (push '("#+end_latex"         . ? ) prettify-symbols-alist)
    (push '("\\\\"                . ?↵) prettify-symbols-alist)

    (defvar pretty-alist
      (cl-pairlis '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta"
                    "theta" "iota" "kappa" "lambda" "mu" "nu" "xi"
                    "omicron" "pi" "rho" "sigma_final" "sigma" "tau"
                    "upsilon" "phi" "chi" "psi" "omega")
                  (mapcar
                   (lambda (x) (make-char 'greek-iso8859-7 x))
                   (number-sequence 97 121))))
    (add-to-list 'pretty-alist '("rangle"   . ?\⟩))
    (add-to-list 'pretty-alist '("Gamma"    . 915))
    (add-to-list 'pretty-alist '("Delta"    . 916))
    (add-to-list 'pretty-alist '("vepsilon" . ?ε))
    (add-to-list 'pretty-alist '("Vepsilon" . ?Ɛ))
    (add-to-list 'pretty-alist '("Theta"    . 920))
    (add-to-list 'pretty-alist '("Lambda"   . 923))
    (add-to-list 'pretty-alist '("Xi"       . 926))
    (add-to-list 'pretty-alist '("Pi"       . 928))
    (add-to-list 'pretty-alist '("Sigma"    . 931))
    (add-to-list 'pretty-alist '("Upsilon"  . 933))
    (add-to-list 'pretty-alist '("vphi"     . ?φ))
    (add-to-list 'pretty-alist '("Phi"      . 934))
    (add-to-list 'pretty-alist '("Psi"      . 936))
    (add-to-list 'pretty-alist '("Omega"    . 937))
    (add-to-list 'pretty-alist '("mp"       . ?∓))
    (add-to-list 'pretty-alist '("pm"       . ?±))
    (add-to-list 'pretty-alist '("to"       . 8594))
    (add-to-list 'pretty-alist '("div"      . 247))
    (add-to-list 'pretty-alist '("ll"       . 8810))
    (add-to-list 'pretty-alist '("gg"       . 8811))
    (add-to-list 'pretty-alist '("leq"      . 8804))
    (add-to-list 'pretty-alist '("geq"      . 8805))
    (add-to-list 'pretty-alist '("neq"      . 8800))
    (add-to-list 'pretty-alist '("sim"      . 8764))
    (add-to-list 'pretty-alist '("approx"   . 8776))
    (add-to-list 'pretty-alist '("infty"    . 8734))
    (add-to-list 'pretty-alist '("perp"     . 8869))
    (add-to-list 'pretty-alist '("parallel" . 8741))
    (add-to-list 'pretty-alist '("angle"    . 8736))
    (add-to-list 'pretty-alist '("triangle" . ?Δ))
    (add-to-list 'pretty-alist '("degree"   . 9900))
    (add-to-list 'pretty-alist '("ua"       . 8593))
    (add-to-list 'pretty-alist '("da"       . 8595))
    (add-to-list 'pretty-alist '("uua"      . 8648))
    (add-to-list 'pretty-alist '("uda"      . 8645))
    (add-to-list 'pretty-alist '("forall"   . 8704))
    (add-to-list 'pretty-alist '("exists"   . 8707))
    (add-to-list 'pretty-alist '("l("       . ?\())
    (add-to-list 'pretty-alist '("r)"       . ?\)))
    (add-to-list 'pretty-alist '("equiv"    . ?≡))
    (add-to-list 'pretty-alist '("cdot"     . 8901))
    (add-to-list 'pretty-alist '("prec"     . ?≺))
    (add-to-list 'pretty-alist '("succ"     . ?≻))

    (mapc
     (lambda (x)
       (let ((word (car x))
             (char (cdr x)))
         (font-lock-add-keywords
          nil
          `((,(concat "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
             (0 (progn
                  (decompose-region (match-beginning 2) (match-end 2))
                  nil)))))
         (font-lock-add-keywords
          nil
          `((,(concat "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
             (0 (progn
                  (compose-region (1- (match-beginning 2)) (match-end 2)
                                  ,char)
                  nil)))))))
     pretty-alist))

  (defun my/latex-load-prettify-symbols ()
    (interactive)
    (push '("\\pm"       . ?±)  prettify-symbols-alist)
    (push '("\\mp"       . ?∓)  prettify-symbols-alist)
    (push '("\\l("       . ?\() prettify-symbols-alist)
    (push '("\\r)"       . ?\)) prettify-symbols-alist)
    (push '("\\["        . ?\[) prettify-symbols-alist)
    (push '("\\]"        . ?\]) prettify-symbols-alist)
    (push '("\\{"        . ?\{) prettify-symbols-alist)
    (push '("\\}"        . ?\}) prettify-symbols-alist)
    (push '("\\l|"       . ?|)  prettify-symbols-alist)
    (push '("\\r|"       . ?|)  prettify-symbols-alist)
    (push '("\\Bigl("    . ?\() prettify-symbols-alist)
    (push '("\\Bigr)"    . ?\)) prettify-symbols-alist)
    (push '("\\Bigl["    . ?\[) prettify-symbols-alist)
    (push '("\\Bigr]"    . ?\]) prettify-symbols-alist)
    (push '("\\Bigl\\{"  . ?\{) prettify-symbols-alist)
    (push '("\\Bigr\\}"  . ?\}) prettify-symbols-alist)
    (push '("\\N"        . ?ℕ) prettify-symbols-alist)
    (push '("\\Z"        . ?ℤ) prettify-symbols-alist)
    (push '("\\Q"        . ?ℚ) prettify-symbols-alist)
    (push '("\\R"        . ?ℝ) prettify-symbols-alist)
    (push '("\\C"        . ?ℂ) prettify-symbols-alist)
    (push '("\\not\\in"  . ?∉) prettify-symbols-alist)
    (push '("\\ua"       . 8593) prettify-symbols-alist)
    (push '("\\da"       . 8595) prettify-symbols-alist)
    (push '("\\uua"      . ?⇈) prettify-symbols-alist)
    (push '("\\uda"      . ?⇅) prettify-symbols-alist)
    (push '("\\Lra"      . ?⇔) prettify-symbols-alist)
    (push '("\\La"       . ?⇐) prettify-symbols-alist)
    (push '("\\Ra"       . ?⇒) prettify-symbols-alist)
    (push '("\\Sqrt"     . ?√) prettify-symbols-alist)
    (push '("\\div"      . ?÷) prettify-symbols-alist)
    (push '("\\degree"   . ?⚬) prettify-symbols-alist)
    (push '("\\triangle" . ?Δ) prettify-symbols-alist)
    (push '("\\vepsilon" . ?ε) prettify-symbols-alist)
    (push '("\\Vepsilon" . ?Ɛ) prettify-symbols-alist)
    (push '("\\vphi"     . ?φ) prettify-symbols-alist)
    (push '("\\\\"       . ?↵) prettify-symbols-alist)))

(provide 'init-edit)
