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
                    "tm"  (lambda ()
                            (interactive)
                            (if (my/current-line-empty-p)
                                (progn
                                  (yas-expand-snippet (yas-lookup-snippet "theorem"))
                                  (my/delete-one-blank-line))
                              (insert "tm")))
                    "еь"  (lambda ()
                            (interactive)
                            (if (my/current-line-empty-p)
                                (progn
                                  (yas-expand-snippet (yas-lookup-snippet "theorem"))
                                  (my/delete-one-blank-line))
                              (insert "еь")))
                    "lm" (lambda ()
                           (interactive)
                           (if (my/current-line-empty-p)
                               (progn
                                 (yas-expand-snippet (yas-lookup-snippet "lemma"))
                                 (my/delete-one-blank-line))
                             (insert "lm")))
                    "дь" (lambda ()
                           (interactive)
                           (if (my/current-line-empty-p)
                               (progn
                                 (yas-expand-snippet (yas-lookup-snippet "lemma"))
                                 (my/delete-one-blank-line))
                             (insert "дь")))
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
                            (yas-expand-snippet "\\\\($1\\\\)$0")
                            (add-hook 'yas/after-exit-snippet-hook 'my/change-lang-in-snippet 0 t))
                    "dm"  (lambda ()
                            (interactive)
                            (if (my/current-line-empty-p)
                                (progn
                                  (yas-expand-snippet (yas-lookup-snippet "display math"))
                                  (my/delete-one-blank-line))
                              (insert "dm")))
                    "вь"  (lambda ()
                            (interactive)
                            (if (my/current-line-empty-p)
                                (progn
                                  (yas-expand-snippet (yas-lookup-snippet "display math"))
                                  (my/delete-one-blank-line))
                              (insert "вь")))
                    ";fg" (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "figure")))
                    "жап" (lambda ()
                            (interactive)
                            (shell-command-to-string "xdotool key Mode_switch")
                            (yas-expand-snippet (yas-lookup-snippet "figure")))
                    ";im" (lambda ()
                            (interactive)
                            (my/insert-image))
                    "жшь" (lambda ()
                            (interactive)
                            (my/insert-image))
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
                    "\\{" (lambda ()
                            (interactive)
                            (yas-expand-snippet "\\\\{$1\\\\}$0")))

  (aas-set-snippets 'latex-mode
                    :cond #'texmathp
                    "'k"    "кг"
                    "'g"    "г"
                    "'m"    "м"
                    "'c"    "см"
                    "'N"    "Н"
                    "'P"    "Па"
                    "'J"    "Дж"
                    "'H"    "Гц"
                    "'s"    "с"
                    "'K"    "К"
                    "'r"    "рад"
                    "'l"    "моль"
                    "'A"    "А"
                    "'V"    "В"
                    "'O"    "Ом"
                    "'C"    "Кл"
                    "'W"    "Вт"
                    "'T"    "Тл"
                    "'F"    "Ф"
                    "'w"    "Вб")

  (aas-set-snippets 'latex-mode
                    :cond #'texmathp
                    "*"     "\\cdot "
                    "%"     "\\%"
                    "  "    "\\ "
                    "xx"    "\\times "
                    "..."   "\\ldots "
                    "->"    "\\to "
                    "=="    "&="
                    "=>"    "\\Ra "
                    "=<"    "\\La "
                    ">>"    "\\gg "
                    "<<"    "\\ll "
                    "<>"    "\\Lra "
                    "<="    "\\leq "
                    ">="    "\\geq "
                    ";="    "\\neq "
                    "pm"    "\\pm "
                    "mp"    "\\mp "
                    "~~"    "\\sim "
                    "~="    "\\approx "
                    "ml"    "\\models "
                    ";a"    "\\alpha "
                    ";b"    "\\beta "
                    ";g"    "\\gamma "
                    ";G"    "\\Gamma "
                    ";d"    "\\delta "
                    ";D"    "\\Delta "
                    ";e"    "\\varepsilon "
                    ";E"    "\\Varepsilon "
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
                    "cot"   "\\cot "
                    "csc"   "\\csc "
                    "csec"  "\\cosec "
                    "exp"   "\\exp"
                    "ln"    "\\ln "
                    "per"   "\\perp "
                    "par"   "\\parallel "
                    "ang"   "\\angle "
                    "min"   "\\min "
                    "max"   "\\max "
                    "sgn"   "\\sgn "
                    "inn"   "\\in "
                    "cup"   "\\cup "
                    "cap"   "\\cap "
                    "bcup"  "\\bigcup "
                    "bcap"  "\\bigcap "
                    "notin" "\\not\\in "
                    "cc"    "\\subset "
                    "dg"    "\\degree "
                    "vd"    "\\vdots "
                    "vv"    "\\vee "
                    "nvv"   "\\wedge "
                    "qq"    "\\quad "
                    "md"    "\\mathrm{d} "
                    "eq"    "\\equiv "
                    "ds"    "\\ds "
                    "ua"    "\\ua "
                    "da"    "\\da "
                    "uua"   "\\uua "
                    "uda"   "\\uda "
                    "rad"   "\\rad "
                    "AA"    "\\forall "
                    "EE"    "\\exists "
                    "nEE"   "\\nexists "
                    "NN"    "\\N"
                    "ZZ"    "\\Z"
                    "QQ"    "\\Q"
                    "RR"    "\\R"
                    "CC"    "\\C"
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
                              (yas-expand-snippet "\\oline{$1}$0"))
                    "obr"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\obr{$1}^{$2}$0"))
                    "ubr"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\ubr{$1}_{$2}$0"))
                    ";T"    (lambda ()
                              (interactive)
                              (shell-command-to-string "xkb-switch -n")
                              (yas-expand-snippet "\\text{$1}$0")
                              (add-hook 'yas/after-exit-snippet-hook 'my/change-lang-in-snippet 0 t))
                    "vc"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\vec{$1}$0"))
                    "brv"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\autobrv{$1}$0"))
                    "box"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\boxed{$1}$0"))
                    "sq"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\Sqrt{$1}$0"))
                    "Sq"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\Sqrt[$1]{$2}$0"))
                    "log"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\log_{$1}$0"))
                    "tri"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\triangle ${1:ABC}$0"))
                    "cas"   (lambda ()
                              (interactive)
                              (if (not (my/current-line-empty-p))
                                  (evil-open-below 1))
                              (yas-expand-snippet "\\begin{dcases}\n$1\\\\\\\\$2\n\\end{dcases}$0"))
                    "scs"   (lambda ()
                              (interactive)
                              (if (not (my/current-line-empty-p))
                                  (evil-open-below 1))
                              (yas-expand-snippet "\\begin{scases}\n$1\\\\\\\\$2\n\\end{scases}$0"))
                    "rcs"   (lambda ()
                              (interactive)
                              (if (not (my/current-line-empty-p))
                                  (evil-open-below 1))
                              (yas-expand-snippet "\\begin{drcases}\n$1\\\\\\\\$2\n\\end{drcases}$0"))
                    "tag"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\tag{$0}"))
                    "rng"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "${1:1},${2:2},\\ldots,${3:n}$0"))
                    "ilrr"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\Bigl(${1:-\\infty},\\ ${2:+\\infty}\\Bigr)$0"))
                    "ilss"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\Bigl[$1,\\ $2\\Bigr]$0"))
                    "ilrs"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\Bigl(${1:-\\infty},\\ $2\\Bigr]$0"))
                    "ilsr"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\Bigl[$1,\\ ${2:+\\infty}\\Bigr)$0"))
                    "binom" (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\binom{${1:n}}{${2:k}}"))
                    "sum"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\sum_{${1:i=1}}^{${2:n}}$0"))
                    "prod"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\prod_{$1}^{$2}$0"))
                    "cprod" (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\coprod_{$1}^{$2}$0"))
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
                    "uex"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\explain{$1}{$2}$0"))
                    "oex"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\explainup{$1}{$2}$0"))
                    "sin"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\sin ${1:\\alpha}"))
                    "cos"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\cos ${1:\\alpha}"))
                    "tg"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\tg ${1:\\alpha}"))
                    "ctg"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\ctg ${1:\\alpha}"))
                    "|"     (lambda ()
                              (interactive)
                              (yas-expand-snippet "|$1|$0"))
                    ";;"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\left($1\\right)$0"))
                    ";["    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\left[$1\\right]$0")
                              (delete-char))
                    ";{"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\left\\\\{$1\\right\\\\}$0"))
                    ";|"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\left|$1\\right|$0")
                              (delete-char))))

(provide 'init-snippets)
