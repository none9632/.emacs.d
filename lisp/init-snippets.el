;; -*- lexical-binding: t -*-

(require 'warnings)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

  (defun my/delete-one-blank-line ()
    (interactive)
    (save-excursion
      (if (re-search-forward "^[[:space:]]*$" nil t)
          (kill-whole-line)))))

(defun my/change-lang-in-snippet ()
  (shell-command-to-string "xkb-switch -n")
  (remove-hook 'yas/after-exit-snippet-hook 'my/change-lang-in-snippet t))

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'org-mode
                    ";lb" (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "latex block"))
                            (my/delete-one-blank-line))
                    "жди" (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "latex block"))
                            (my/delete-one-blank-line))
                    "tm"  (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "theorem"))
                            (my/delete-one-blank-line))
                    "еь"  (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "theorem"))
                            (my/delete-one-blank-line))
                    "lm"  (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "lemma"))
                            (my/delete-one-blank-line))
                    "дь"  (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "lemma"))
                            (my/delete-one-blank-line))
                    "mk"  (lambda ()
                            (interactive)
                            (yas-expand-snippet "\\\\($0\\\\)")
                            (my/org-edit-special)
                            (evil-insert-state)
                            (search-forward "(" nil t nil))
                    "ьл"  (lambda ()
                            (interactive)
                            (yas-expand-snippet "\\\\($0\\\\)")
                            (my/org-edit-special)
                            (evil-insert-state)
                            (search-forward "(" nil t nil))
                    "ms"  (lambda ()
                            (interactive)
                            (yas-expand-snippet "\\\\($1\\\\)$0"))
                    "ьы"  (lambda ()
                            (interactive)
                            (shell-command-to-string "xdotool key Mode_switch")
                            (yas-expand-snippet "\\\\($1\\\\)$0"))
                    "dm"  (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "display math"))
                            (my/delete-one-blank-line))
                    "вь"  (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "display math"))
                            (my/delete-one-blank-line))
                    "gr"  (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "gather"))
                            (my/delete-one-blank-line))
                    "пк"  (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "gather"))
                            (my/delete-one-blank-line))
                    ";fn" (lambda ()
                            (interactive)
                            (yas-expand-snippet "[fn::$1]$0"))
                    ";fg" (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "figure")))
                    "жап" (lambda ()
                            (interactive)
                            (shell-command-to-string "xdotool key Mode_switch")
                            (yas-expand-snippet (yas-lookup-snippet "figure")))
                    ";im" (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "image")))
                    "жшь" (lambda ()
                            (interactive)
                            (shell-command-to-string "xdotool key Mode_switch")
                            (yas-expand-snippet (yas-lookup-snippet "image")))
                    ";cp" (lambda ()
                            (interactive)
                            (yas-expand-snippet "#+caption: $0")
                            (shell-command-to-string "xkb-switch -n"))
                    "жсз" (lambda ()
                            (interactive)
                            (yas-expand-snippet "#+caption: $0"))
                    "\\\\" (lambda ()
                             (interactive)
                             (yas-expand-snippet " \\\\\\\\")))

  (aas-set-snippets 'latex-mode
                    "mk"  (lambda ()
                            (interactive)
                            (yas-expand-snippet "$$1$"))
                    "ьл"  (lambda ()
                            (interactive)
                            (shell-command-to-string "xdotool key Mode_switch")
                            (yas-expand-snippet "$$1$")
                            (add-hook 'yas/after-exit-snippet-hook 'my/change-lang-in-snippet 0 t))
                    "dm"  (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "align"))
                            (my/delete-one-blank-line))
                    "вь"  (lambda ()
                            (interactive)
                            (shell-command-to-string "xdotool key Mode_switch")
                            (yas-expand-snippet (yas-lookup-snippet "align"))
                            (my/delete-one-blank-line))
                    "gr"  (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "gather"))
                            (my/delete-one-blank-line))
                    "пк"  (lambda ()
                            (interactive)
                            (shell-command-to-string "xdotool key Mode_switch")
                            (yas-expand-snippet (yas-lookup-snippet "gather"))
                            (my/delete-one-blank-line))
                    "("   (lambda ()
                            (interactive)
                            (yas-expand-snippet "($1)$0"))
                    "["   (lambda ()
                            (interactive)
                            (yas-expand-snippet "[$1]$0"))
                    "\\{" (lambda ()
                            (interactive)
                            (yas-expand-snippet "\\\\{$1\\\\}$0")))

  (aas-set-snippets 'latex-mode
                    :cond #'texmathp
                    "*"     "\\cdot "
                    "  "    "\\ "
                    "xx"    "\\times "
                    "..."   "\\ldots "
                    "->"    "\\to "
                    "=="    "&="
                    "=>"    "\\Rightarrow "
                    "=<"    "\\Leftarrow "
                    ">>"    "\\gg "
                    "<<"    "\\ll "
                    "<>"    "\\Leftrightarrow "
                    "<="    "\\leq "
                    ">="    "\\geq "
                    ";="    "\\neq "
                    "pm"    "\\pm "
                    "mp"    "\\mp "
                    "~~"    "\\sim "
                    "~="    "\\approx "
                    "||"    "\\mid "
                    "|="    "\\models "
                    ";a"    "\\alpha "
                    ";b"    "\\beta "
                    ";g"    "\\gamma "
                    ";G"    "\\Gamma "
                    ";d"    "\\delta "
                    ";D"    "\\Delta "
                    ";e"    "\\varepsilon "
                    ";z"    "\\zeta "
                    ";h"    "\\eta "
                    ";q"    "\\theta "
                    ";Q"    "\\Theta "
                    ";i"    "\\iota "
                    ";k"    "\\kappa "
                    ";l"    "\\lambda "
                    ";L"    "\\Lambda "
                    ";m"    "\\mu "
                    ";n"    "\\nu "
                    ";x"    "\\xi "
                    ";X"    "\\Xi "
                    ";p"    "\\pi "
                    ";P"    "\\Pi "
                    ";r"    "\\rho "
                    ";s"    "\\sigma "
                    ";S"    "\\Sigma "
                    ";t"    "\\tau "
                    ";u"    "\\upsilon "
                    ";U"    "\\Upsilon "
                    ";f"    "\\varphi "
                    ";F"    "\\Phi "
                    ";o"    "\\chi "
                    ";y"    "\\psi "
                    ";Y"    "\\Psi "
                    ";w"    "\\omega "
                    ";W"    "\\Omega "
                    ";8"    "\\infty "
                    ";0"    "\\emptyset "
                    "acos"  "\\arccos "
                    "acot"  "\\arccot "
                    "acsc"  "\\arccsc "
                    "asec"  "\\arcsec "
                    "asin"  "\\arcsin "
                    "atan"  "\\arctan "
                    "atg"   "\\arctg "
                    "tg"    "\\tg "
                    "ctg"   "\\ctg "
                    "cos"   "\\cos "
                    "cot"   "\\cot "
                    "csc"   "\\csc "
                    "csec"  "\\cosec "
                    "sin"   "\\sin "
                    "exp"   "\\exp"
                    "ln"    "\\ln "
                    "log"   "\\log "
                    "perp"  "\\perp"
                    "min"   "\\min "
                    "max"   "\\max "
                    "inn"   "\\in "
                    "cup"   "\\cup "
                    "cap"   "\\cap "
                    "bcup"  "\\bigcup "
                    "bcap"  "\\bigcap "
                    "notin" "\\not\\in "
                    "cc"    "\\subset "
                    "dg"    "\\degree "
                    "vd"    "\\ \\vdots\\ "
                    "vv"    "\\vee "
                    "nvv"   "\\wedge "
                    "qq"    "\\quad "
                    "md"    "\\mathrm{d} "
                    "eq"    "\\equiv "
                    "ds"    "\\displaystyle "
                    "AA"    "\\forall "
                    "EE"    "\\exists "
                    "nEE"   "\\nexists "
                    "NN"    "\\mathbb{N}"
                    "ZZ"    "\\mathbb{Z}"
                    "QQ"    "\\mathbb{Q}"
                    "RR"    "\\mathbb{R}"
                    "CC"    "\\mathbb{C}"
                    "sr"    "^2"
                    "cb"    "^3"
                    "inv"   "^{-1}"
                    "td"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "^{$1}$0"))
                    "^"     (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\hat{$1}$0"))
                    "_"     (lambda ()
                              (interactive)
                              (yas-expand-snippet "_{$1}$0"))
                    "//"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\frac{$1}{$2}$0"))
                    "dd"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\frac{\\mathrm{d} $1}{\\mathrm{d} ${2:t}}$0"))
                    "prt"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\frac{\\partial $1}{\\partial ${2:x}}$0"))
                    "set"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\set{$1}{$2}$0"))
                    "mod"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\Mod{$1}$0"))
                    "bar"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\bar{$1}$0"))
                    "oln"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\overline{$1}$0"))
                    "uln"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\overline{$1}$0"))
                    "obr"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\overbrace{$1}^{$2}$0"))
                    "ubr"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\underbrace{$1}_{$2}$0"))
                    ";T"    (lambda ()
                              (interactive)
                              (shell-command-to-string "xkb-switch -n")
                              (yas-expand-snippet "\\T{$1} $0")
                              (add-hook 'yas/after-exit-snippet-hook 'my/change-lang-in-snippet 0 t))
                    "vc"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\vec{$1}$0"))
                    "sq"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\sqrt{$1}$0"))
                    "Sq"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\sqrt[$1]{$2}$0"))
                    "cas"   (lambda ()
                              (interactive)
                              (if (not (my/current-line-empty-p))
                                  (evil-open-below 1))
                              (yas-expand-snippet "\\begin{cases}\n$1\\\\\\\\\n    $2\n\\end{cases}$0"))
                    "scs"   (lambda ()
                              (interactive)
                              (if (not (my/current-line-empty-p))
                                  (evil-open-below 1))
                              (yas-expand-snippet "\\begin{scases}\n$1\\\\\\\\\n    $2\n\\end{scases}$0"))
                    "tg"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\tag{$0}"))
                    "rng"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "${1:1},${2:2},\\ldots,${3:n}$0"))
                    "ilrr"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\left(${1:-\\infty},\\ ${2:+\\infty}\\right)$0"))
                    "ilss"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\left[$1,\\ $2\\right]$0"))
                    "ilrs"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\left(${1:-\\infty},\\ $2\\right]$0"))
                    "ilsr"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\left[$1,\\ ${2:+\\infty}\\right)$0"))
                    "binom" (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\binom{${1:n}}{${2:k}}"))
                    "sum"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\sum_{$1}^{$2}$0"))
                    "prod"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\prod_{$1}^{$2}$0"))
                    "cprod" (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\cprod_{$1}^{$2}$0"))
                    "lim"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\lim_{${1:n} \\to ${2:\\infty}}$0"))
                    "int"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\int_{${1:-\\infty}}^{${2:\\infty}}$0"))
                    "ing"   (lambda ()
                              (interactive)
                              (yas-expand-snippet (yas-lookup-snippet "integ"))
                              (my/delete-one-blank-line))
                    "mat"   (lambda ()
                              (interactive)
                              (yas-expand-snippet (yas-lookup-snippet "matrix"))
                              (my/delete-one-blank-line))
                    ";;"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\left($1\\right)$0"))
                    ";["    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\left[$1\\right]$0")
                              (delete-char))
                    ";{"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\left\\\\{$1\\right\\\\}$0"))))

(provide 'init-snippets)
