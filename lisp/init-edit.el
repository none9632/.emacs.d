;; -*- lexical-binding: t -*-

(require 'init-evil)
(require 'init-basic)

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method 'russian-computer)

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
  (defvar my/exclude-electic-pair-modes '(LaTeX-mode))

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
  (defun my/tex--prettify-symbols-compose-p (_start end _match)
    (or
     ;; If the matched symbol doesn't end in a word character, then we
     ;; simply allow composition.  The symbol is probably something like
     ;; \|, \(, etc.
     (not (eq ?w (char-syntax (char-before end))))
     ;; Else we look at what follows the match in order to decide.
     (let* ((after-char (char-after end))
            (after-syntax (char-syntax after-char)))
       (not (or
             ;; Don't compose \alpha@foo.
             (eq after-char ?@)
             ;; The \alpha in \alpha2 or \alpha-\beta may be composed but
             ;; of course \alphax may not.
             (and (eq after-syntax ?w)
                  (not (memq after-char
                             '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?+ ?- ?' ?\"))))
             ;; Don't compose inside verbatim blocks.
             (eq 2 (nth 7 (syntax-ppss))))))))

  (defun my/org-load-prettify-symbols ()
    (interactive)
    (set-face-background 'org-block-begin-line "#282c34")

    (defvar my/org-latex-prettify-symbols-alist
      '(("\\alpha" . ?α)
        ("\\beta" . ?β)
        ("\\gamma" . ?γ)
        ("\\delta" . ?δ)
        ("\\epsilon" . ?ϵ)
        ("\\zeta" . ?ζ)
        ("\\eta" . ?η)
        ("\\theta" . ?θ)
        ("\\iota" . ?ι)
        ("\\kappa" . ?κ)
        ("\\lambda" . ?λ)
        ("\\mu" . ?μ)
        ("\\nu" . ?ν)
        ("\\xi" . ?ξ)
        ("\\pi" . ?π)
        ("\\rho" . ?ρ)
        ("\\sigma" . ?σ)
        ("\\tau" . ?τ)
        ("\\upsilon" . ?υ)
        ("\\phi" . ?ϕ)
        ("\\chi" . ?χ)
        ("\\psi" . ?ψ)
        ("\\omega" . ?ω)
        ("\\Gamma" . ?Γ)
        ("\\Delta" . ?Δ)
        ("\\Lambda" . ?Λ)
        ("\\Phi" . ?Φ)
        ("\\Pi" . ?Π)
        ("\\Psi" . ?Ψ)
        ("\\Sigma" . ?Σ)
        ("\\Theta" . ?Θ)
        ("\\Upsilon" . ?Υ)
        ("\\Xi" . ?Ξ)
        ("\\Omega" . ?Ω)
        ;; Other math symbols
        ("\\angle" . ?∠)
        ("\\approx" . ?≈)
        ("\\backcong" . ?≌)
        ("\\backepsilon" . ?∍)
        ("\\backsim" . ?∽)
        ("\\backsimeq" . ?⋍)
        ("\\bigcap" . ?⋂)
        ("\\bigcup" . ?⋃)
        ("\\bot" . ?⊥)
        ("\\bullet" . ?•)
        ("\\cap" . ?∩)
        ("\\cdot" . ?⋅)
        ("\\circ" . ?∘)
        ("\\cong" . ?≅)
        ("\\cup" . ?∪)
        ("\\emptyset" . ?∅)
        ("\\equiv" . ?≡)
        ("\\exists" . ?∃)
        ("\\ge" . ?≥)
        ("\\geq" . ?≥)
        ("\\geqq" . ?≧)
        ("\\geqslant" . ?≥)
        ("\\gg" . ?≫)
        ("\\in" . ?∈)
        ("\\infty" . ?∞)
        ("\\int" . ?∫)
        ("\\langle" . 10216)          ; Literal ?⟨ breaks indentation.
        ("\\lbrace" . ?{)
        ("\\lbrack" . ?\[)
        ("\\ldots" . ?…)
        ("\\le" . ?≤)
        ("\\leftarrow" . ?←)
        ("\\leftleftarrows" . ?⇇)
        ("\\leftrightarrow" . ?↔)
        ("\\leftrightarrows" . ?⇆)
        ("\\leq" . ?≤)
        ("\\leqslant" . ?≤)
        ("\\longleftarrow" . ?←)
        ("\\longleftrightarrow" . ?↔)
        ("\\longmapsto" . ?↦)
        ("\\longrightarrow" . ?→)
        ("\\mapsto" . ?↦)
        ("\\mid" . ?∣)
        ("\\mp" . ?∓)
        ("\\nabla" . ?∇)
        ("\\ne" . ?≠)
        ("\\neq" . ?≠)
        ("\\nequiv" . ?≢)
        ("\\nexists" . ?∄)
        ("\\ngeq" . ?≱)
        ("\\ngeqq" . ?≱)
        ("\\ngeqslant" . ?≱)
        ("\\ngtr" . ?≯)
        ("\\ni" . ?∋)
        ("\\notin" . ?∉)
        ("\\oint" . ?∮)
        ("\\oplus" . ?⊕)
        ("\\otimes" . ?⊗)
        ("\\parallel" . ?∥)
        ("\\partial" . ?∂)
        ("\\perp" . ?⊥)
        ("\\prod" . ?∏)
        ("\\rangle" . 10217)            ; Literal ?⟩ breaks indentation.
        ("\\rbrace" . ?})
        ("\\rbrack" . ?\])
        ("\\rightarrow" . ?→)
        ("\\rightleftarrows" . ?⇄)
        ("\\rightrightarrows" . ?⇉)
        ("\\setminus" . ?∖)
        ("\\sim" . ?∼)
        ("\\simeq" . ?≃)
        ("\\smallsetminus" . ?∖)
        ("\\star" . ?⋆)
        ("\\straightphi" . ?φ)
        ("\\subset" . ?⊂)
        ("\\subseteq" . ?⊆)
        ("\\sum" . ?∑)
        ("\\supset" . ?⊃)
        ("\\supseteq" . ?⊇)
        ("\\thickapprox" . ?≈)
        ("\\thicksim" . ?∼)
        ("\\to" . ?→)
        ("\\uparrow" . ?↑)
        ("\\upuparrows" . ?⇈)
        ("\\vdots" . ?⋮)
        ("\\pm" . ?±)
        ("--" . ?–)
        ("---" . ?—)))
    (add-function :override (local 'prettify-symbols-compose-predicate)
                  #'my/tex--prettify-symbols-compose-p)
    (setq-local prettify-symbols-alist my/org-latex-prettify-symbols-alist)

    (my/latex-load-prettify-symbols)
    (push '("#+begin_latex LaTeX" . ? ) prettify-symbols-alist)
    (push '("#+end_latex"         . ? ) prettify-symbols-alist))

  (defun my/latex-load-prettify-symbols ()
    (interactive)
    (push '("\\l("       . ?\() prettify-symbols-alist)
    (push '("\\r)"       . ?\)) prettify-symbols-alist)
    (push '("\\{"        . ?\{) prettify-symbols-alist)
    (push '("\\}"        . ?\}) prettify-symbols-alist)
    (push '("\\l\\{"     . ?\{) prettify-symbols-alist)
    (push '("\\r\\}"     . ?\}) prettify-symbols-alist)
    (push '("\\["        . ?\[) prettify-symbols-alist)
    (push '("\\]"        . ?\]) prettify-symbols-alist)
    (push '("\\<"        . ?\⟨) prettify-symbols-alist)
    (push '("\\>"        . ?\⟩) prettify-symbols-alist)
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
    (push '("\\ua"       . 8593) prettify-symbols-alist)
    (push '("\\da"       . 8595) prettify-symbols-alist)
    (push '("\\uua"      . ?⇈) prettify-symbols-alist)
    (push '("\\uda"      . ?⇅) prettify-symbols-alist)
    (push '("\\lla"      . ?⇇) prettify-symbols-alist)
    (push '("\\rra"      . ?⇉) prettify-symbols-alist)
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
    (push '("\\land"     . ?∧) prettify-symbols-alist)
    (push '("\\lor"      . ?∨) prettify-symbols-alist)
    (push '("\\prt"      . ?∂) prettify-symbols-alist)
    (push '("\\\\"       . ?↵) prettify-symbols-alist)
    (push '("\\boplus"   . ?⊕) prettify-symbols-alist)
    (push '("\\botimes"  . ?⊗) prettify-symbols-alist)

    ;; caligraphic
    (push '("\\mcal{A}" . ?𝒜) prettify-symbols-alist)
    (push '("\\mcal{B}" . ?ℬ) prettify-symbols-alist)
    (push '("\\mcal{C}" . ?𝒞) prettify-symbols-alist)
    (push '("\\mcal{D}" . ?𝒟) prettify-symbols-alist)
    (push '("\\mcal{E}" . ?ℰ) prettify-symbols-alist)
    (push '("\\mcal{F}" . ?ℱ) prettify-symbols-alist)
    (push '("\\mcal{G}" . ?𝒢) prettify-symbols-alist)
    (push '("\\mcal{H}" . ?ℋ) prettify-symbols-alist)
    (push '("\\mcal{I}" . ?ℐ) prettify-symbols-alist)
    (push '("\\mcal{J}" . ?𝒥) prettify-symbols-alist)
    (push '("\\mcal{K}" . ?𝒦) prettify-symbols-alist)
    (push '("\\mcal{L}" . ?ℒ) prettify-symbols-alist)
    (push '("\\mcal{M}" . ?ℳ) prettify-symbols-alist)
    (push '("\\mcal{N}" . ?𝒩) prettify-symbols-alist)
    (push '("\\mcal{O}" . ?𝒪) prettify-symbols-alist)
    (push '("\\mcal{P}" . ?𝒫) prettify-symbols-alist)
    (push '("\\mcal{Q}" . ?𝒬) prettify-symbols-alist)
    (push '("\\mcal{R}" . ?ℛ) prettify-symbols-alist)
    (push '("\\mcal{S}" . ?𝒮) prettify-symbols-alist)
    (push '("\\mcal{T}" . ?𝒯) prettify-symbols-alist)
    (push '("\\mcal{U}" . ?𝒰) prettify-symbols-alist)
    (push '("\\mcal{V}" . ?𝒱) prettify-symbols-alist)
    (push '("\\mcal{W}" . ?𝒲) prettify-symbols-alist)
    (push '("\\mcal{X}" . ?𝒳) prettify-symbols-alist)
    (push '("\\mcal{Y}" . ?𝒴) prettify-symbols-alist)
    (push '("\\mcal{Z}" . ?𝒵) prettify-symbols-alist)

    ;; fractur
    (push '("\\mfr{A}" . ?𝔄) prettify-symbols-alist)
    (push '("\\mfr{B}" . ?𝔅) prettify-symbols-alist)
    (push '("\\mfr{C}" . ?ℭ) prettify-symbols-alist)
    (push '("\\mfr{D}" . ?𝔇) prettify-symbols-alist)
    (push '("\\mfr{E}" . ?𝔈) prettify-symbols-alist)
    (push '("\\mfr{F}" . ?𝔉) prettify-symbols-alist)
    (push '("\\mfr{G}" . ?𝔊) prettify-symbols-alist)
    (push '("\\mfr{H}" . ?ℌ) prettify-symbols-alist)
    (push '("\\mfr{I}" . ?ℑ) prettify-symbols-alist)
    (push '("\\mfr{J}" . ?𝔍) prettify-symbols-alist)
    (push '("\\mfr{K}" . ?𝔎) prettify-symbols-alist)
    (push '("\\mfr{L}" . ?𝔏) prettify-symbols-alist)
    (push '("\\mfr{M}" . ?𝔐) prettify-symbols-alist)
    (push '("\\mfr{N}" . ?𝔑) prettify-symbols-alist)
    (push '("\\mfr{O}" . ?𝔒) prettify-symbols-alist)
    (push '("\\mfr{P}" . ?𝔓) prettify-symbols-alist)
    (push '("\\mfr{Q}" . ?𝔔) prettify-symbols-alist)
    (push '("\\mfr{R}" . ?ℜ) prettify-symbols-alist)
    (push '("\\mfr{S}" . ?𝔖) prettify-symbols-alist)
    (push '("\\mfr{T}" . ?𝔗) prettify-symbols-alist)
    (push '("\\mfr{U}" . ?𝔘) prettify-symbols-alist)
    (push '("\\mfr{V}" . ?𝔙) prettify-symbols-alist)
    (push '("\\mfr{W}" . ?𝔚) prettify-symbols-alist)
    (push '("\\mfr{X}" . ?𝔛) prettify-symbols-alist)
    (push '("\\mfr{Y}" . ?𝔜) prettify-symbols-alist)
    (push '("\\mfr{Z}" . ?ℨ) prettify-symbols-alist)
    (push '("\\mfr{a}" . ?𝔞) prettify-symbols-alist)
    (push '("\\mfr{b}" . ?𝔟) prettify-symbols-alist)
    (push '("\\mfr{c}" . ?𝔠) prettify-symbols-alist)
    (push '("\\mfr{d}" . ?𝔡) prettify-symbols-alist)
    (push '("\\mfr{e}" . ?𝔢) prettify-symbols-alist)
    (push '("\\mfr{f}" . ?𝔣) prettify-symbols-alist)
    (push '("\\mfr{g}" . ?𝔤) prettify-symbols-alist)
    (push '("\\mfr{h}" . ?𝔥) prettify-symbols-alist)
    (push '("\\mfr{i}" . ?𝔦) prettify-symbols-alist)
    (push '("\\mfr{j}" . ?𝔧) prettify-symbols-alist)
    (push '("\\mfr{k}" . ?𝔨) prettify-symbols-alist)
    (push '("\\mfr{l}" . ?𝔩) prettify-symbols-alist)
    (push '("\\mfr{m}" . ?𝔪) prettify-symbols-alist)
    (push '("\\mfr{n}" . ?𝔫) prettify-symbols-alist)
    (push '("\\mfr{o}" . ?𝔬) prettify-symbols-alist)
    (push '("\\mfr{p}" . ?𝔭) prettify-symbols-alist)
    (push '("\\mfr{q}" . ?𝔮) prettify-symbols-alist)
    (push '("\\mfr{r}" . ?𝔯) prettify-symbols-alist)
    (push '("\\mfr{s}" . ?𝔰) prettify-symbols-alist)
    (push '("\\mfr{t}" . ?𝔱) prettify-symbols-alist)
    (push '("\\mfr{u}" . ?𝔲) prettify-symbols-alist)
    (push '("\\mfr{v}" . ?𝔳) prettify-symbols-alist)
    (push '("\\mfr{w}" . ?𝔴) prettify-symbols-alist)
    (push '("\\mfr{x}" . ?𝔵) prettify-symbols-alist)
    (push '("\\mfr{y}" . ?𝔶) prettify-symbols-alist)
    (push '("\\mfr{z}" . ?𝔷) prettify-symbols-alist)))

(provide 'init-edit)
