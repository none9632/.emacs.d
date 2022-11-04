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

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :bind ((:map evil-insert-state-map
               ("<escape>" . undo)))
  :config
  (defun my/change-lang-in-snippet ()
    (shell-command-to-string "xkb-switch -n")
    (remove-hook 'yas/after-exit-snippet-hook 'my/change-lang-in-snippet t))

  (defun my/temp-abort-snippet ()
    (yas-abort-snippet)
    (remove-hook 'post-command-hook #'my/temp-abort-snippet))

  (defun my/new-line-snippet ()
    (if (not (my/current-line-empty-p))
        (evil-open-below 1)))

  (defun my/enable-snippets (&optional mode)
    (my/common-snippets 'org-mode)
    (my/common-snippets 'latex-mode)
    (or mode (setq mode nil))
    (if (eq mode 'phys) (my/phys-snippets))
    (if (eq mode 'math) (my/math-snippets)))

  (add-hook 'aas-pre-snippet-expand-hook 'undo-boundary)

  
  (defun my/common-snippets (mode)
    (aas-set-snippets mode
                      :cond #'texmathp
                      "*"     "\\cdot "
                      "'8"    "*"
                      "%"     "\\%"
                      "  "    "\\ "
                      "xx"    "\\times "
                      "..."   "\\ldots "
                      "->"    "\\to "
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
                      ";a"    "\\alpha "
                      ";b"    "\\beta "
                      ";g"    "\\gamma "
                      ";G"    "\\Gamma "
                      ";d"    "\\delta "
                      ";D"    "\\Delta "
                      ";e"    "\\vepsilon "
                      ";E"    "\\Vepsilon "
                      ";z"    "\\zeta "
                      ";n"    "\\eta "
                      ";o"    "\\theta "
                      ";O"    "\\Theta "
                      ";i"    "\\iota "
                      ";k"    "\\kappa "
                      ";l"    "\\lambda "
                      ";L"    "\\Lambda "
                      ";m"    "\\mu "
                      ";v"    "\\nu "
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
                      ";f"    "\\vphi "
                      ";F"    "\\Phi "
                      ";q"    "\\chi "
                      ";y"    "\\psi "
                      ";Y"    "\\Psi "
                      ";w"    "\\omega "
                      ";W"    "\\Omega "
                      ";8"    "\\infty "
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
                      "dg"    "\\degree "
                      "vd"    "\\vdots "
                      "qq"    "\\quad "
                      "ua"    "\\ua "
                      "da"    "\\da "
                      "uua"   "\\uua "
                      "uda"   "\\uda "
                      "div"   "\\div "
                      "sr"    "^2"
                      "cb"    "^3"
                      "inv"   "^{-1}")
  
    (aas-set-snippets mode
                      :cond #'texmathp
                      "("     (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\\\l($1\\\\r)$0"))
                      "["     (lambda ()
                                (interactive)
                                (yas-expand-snippet "[$1]$0"))
                      "{"     (lambda ()
                                (interactive)
                                (yas-expand-snippet "{$1}$0"))
                      "|"     (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\\\l|$1\\\\r|$0"))
                      ";{"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\\\l\\\\{$1\\\\r\\\\}$0"))
                      ";["    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\\\l[$1\\\\r]$0"))
                      ";("    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\l("))
                      ";)"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\r)"))
                      ";]"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\r]"))
                      ";}"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\\\}$0")))
  
    (aas-set-snippets mode
                      :cond #'texmathp
                      "td"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "^{$1}"))
                      "//"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\frac{$1}{$2}$0"))
                      "vc"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\vec{$1}$0"))
                      "sq"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\Sqrt{$1}$0"))
                      "Sq"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\Sqrt[$1]{$2}$0"))
                      "tri"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\triangle ${1:ABC}$0")
                                (add-hook 'post-command-hook #'my/temp-abort-snippet))
                      "sin"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\sin ${1:\\alpha }")
                                (add-hook 'post-command-hook #'my/temp-abort-snippet))
                      "cos"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\cos ${1:\\alpha }")
                                (add-hook 'post-command-hook #'my/temp-abort-snippet))
                      "tg"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\tg ${1:\\alpha }")
                                (add-hook 'post-command-hook #'my/temp-abort-snippet))
                      "ctg"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\ctg ${1:\\alpha }")
                                (add-hook 'post-command-hook #'my/temp-abort-snippet))
                      "mrm"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\mrm{$1}$0"))
                      "mf"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\mbf{$1}$0"))
                      "mbb"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\mbb{$1}$0"))
                      "mds"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\mds{$1}$0"))
                      "mcl"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\mcal{$1}$0"))
                      "mcr"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\mscr{$1}$0"))))
  
  
  (defun my/math-snippets ()
    (aas-set-snippets 'latex-mode
                      :cond #'texmathp
                      ";\\"   "\\setminus "
                      ";->"   "\\mapsto "
                      "-<"    "\\prec "
                      ">-"    "\\succ "
                      "=="    "\\eqdef "
                      "ml"    "\\models "
                      "cir"   "\\circ "
                      "sgn"   "\\sgn "
                      "inn"   "\\in "
                      "cup"   "\\cup "
                      "cap"   "\\cap "
                      "bcup"  "\\bigcup "
                      "bcap"  "\\bigcap "
                      "notin" "\\not\\in "
                      "cc"    "\\subset "
                      "c=="   "\\subseteq "
                      "eq"    "\\equiv "
                      "AA"    "\\forall "
                      "EE"    "\\exists "
                      "nEE"   "\\nexists "
                      "land"  "\\land "
                      "lor"   "\\lor "
                      "NN"    "\\N "
                      "ZZ"    "\\Z "
                      "QQ"    "\\Q "
                      "RR"    "\\R "
                      "CC"    "\\C "
                      "ld"    "\\ddots "
                      "det"   "\\det "
                      "set"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\set{$1}{$2}$0"))
                      "mod"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\Mod{$1}$0"))
                      "rng"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "${1:1},${2:2},\\ldots,${3:n}$0")))
  
    (aas-set-snippets 'org-mode
                      :cond #'texmathp
                      ";\\"   "\\setminus "
                      ";->"   "\\mapsto "
                      "-<"    "\\prec "
                      ">-"    "\\succ "
                      "cir"   "\\circ "
                      "inn"   "\\in "
                      "cup"   "\\cup "
                      "cap"   "\\cap "
                      "bcup"  "\\bigcup "
                      "bcap"  "\\bigcap "
                      "notin" "\\not\\in "
                      "sup"   "\\sup "
                      "cc"    "\\subset "
                      "c=="   "\\subseteq "
                      "eq"    "\\equiv "
                      "AA"    "\\forall "
                      "EE"    "\\exists "
                      "nEE"   "\\nexists "
                      "land"  "\\land "
                      "lor"   "\\lor "
                      "NN"    "\\N "
                      "ZZ"    "\\Z "
                      "QQ"    "\\Q "
                      "RR"    "\\R "
                      "CC"    "\\C ")
  
    (aas-set-snippets 'latex-mode
                      :cond #'texmathp
                      "binom" (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\binom{${1:n}}{${2:k}}"))
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
                                (yas-expand-snippet "\\Bigl[$1,\\ ${2:+\\infty}\\Bigr)$0"))))
  
  
  (defun my/phys-snippets ()
    (aas-set-snippets 'latex-mode
                      :cond #'texmathp
                      ";h"    "\\hbar "
                      "'k"    "кг"
                      "'g"    "г"
                      "'m"    "м"
                      "'c"    "см"
                      "'N"    "Н"
                      "'P"    "Па"
                      "'J"    "Дж"
                      "'h"    "Гц"
                      "'H"    "Гн"
                      "'s"    "с"
                      "'K"    "К"
                      "'r"    "рад"
                      "'l"    "моль"
                      "'A"    "А"
                      "'V"    "В"
                      "'O"    "Ом"
                      "'C"    "Кл"
                      "'W"    "Вт"
                      "'w"    "Вб"
                      "'T"    "Тл"
                      "'F"    "Ф"
                      ".t"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\dot{$1}$0"))
                      "..t"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\ddot{$1}$0"))))
  
  
  (aas-set-snippets 'latex-mode
                    :cond (lambda ()
                            (not (texmathp)))
                    "mk"  (lambda ()
                            (interactive)
                            (yas-expand-snippet "$$1$"))
                    "ьл"  (lambda ()
                            (interactive)
                            (shell-command-to-string "xdotool key Mode_switch")
                            (yas-expand-snippet "$$1$")
                            (add-hook 'yas/after-exit-snippet-hook 'my/change-lang-in-snippet 0 t))
                    "ms"  (lambda ()
                            (interactive)
                            (yas-expand-snippet "$$1$"))
                    "ьы"  (lambda ()
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
                            (my/delete-one-blank-line)))
  
  (aas-set-snippets 'latex-mode
                    :cond #'texmathp
                    "^"     (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\hat{$1}$0"))
                    "_"     (lambda ()
                              (interactive)
                              (yas-expand-snippet "_{$1}"))
                    "/dd"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\frac{d $1}{d ${2:t}}$0"))
                    "prt"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\frac{\\partial $1}{\\partial ${2:x}}$0"))
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
                              (add-hook 'yas/after-exit-snippet-hook 'my/change-lang-in-snippet 0 t)
                              )
                    "brv"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\autobrv{$1}$0"))
                    "box"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\boxed{$1}$0"))
                    "log"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\log_{$1}$0"))
                    "cas"   (lambda ()
                              (interactive)
                              (my/new-line-snippet)
                              (yas-expand-snippet "\\begin{dcases}\n$1\\\\\\\\$2\n\\end{dcases}$0"))
                    "scs"   (lambda ()
                              (interactive)
                              (my/new-line-snippet)
                              (yas-expand-snippet "\\begin{scases}\n$1\\\\\\\\$2\n\\end{scases}$0"))
                    "rcs"   (lambda ()
                              (interactive)
                              (my/new-line-snippet)
                              (yas-expand-snippet "\\begin{drcases}\n$1\\\\\\\\$2\n\\end{drcases}$0"))
                    "tag"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\tag{$0}"))
                    "sum"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\sum^{${1:n}}_{${2:i=1}}$0"))
                    "prod"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\prod^{$1}_{$2}$0"))
                    "cprod" (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\coprod^{$1}_{$2}$0"))
                    "lim"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\lim_{${1:n} \\to ${2:\\infty}}$0"))
                    "int"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\int_{${1:-\\infty}}^{${2:\\infty}}$0"))
                    "oin"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\oint_{${1:-\\infty}}^{${2:\\infty}}$0"))
                    ;; "ing"   (lambda ()
                    ;;           (interactive)
                    ;;           (yas-expand-snippet (yas-lookup-snippet "integ"))
                    ;;           (my/delete-one-blank-line))
                    "mat"   (lambda ()
                              (interactive)
                              (my/new-line-snippet)
                              (yas-expand-snippet (yas-lookup-snippet "pmatrix"))
                              (my/delete-one-blank-line))
                    "bmt"   (lambda ()
                              (interactive)
                              (my/new-line-snippet)
                              (yas-expand-snippet (yas-lookup-snippet "bmatrix"))
                              (my/delete-one-blank-line))
                    "vmt"   (lambda ()
                              (interactive)
                              (my/new-line-snippet)
                              (yas-expand-snippet (yas-lookup-snippet "vmatrix"))
                              (my/delete-one-blank-line))
                    "Vmt"   (lambda ()
                              (interactive)
                              (my/new-line-snippet)
                              (yas-expand-snippet (yas-lookup-snippet "Vmatrix"))
                              (my/delete-one-blank-line))
                    "mrm"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\mrm{$1}$0"))
                    "mf"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\mbf{$1}$0"))
                    "mbb"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\mbb{$1}$0"))
                    "mcl"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\mcal{$1}$0"))
                    "mcr"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\mscr{$1}$0"))
                    "stack" (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\substack{$1}$0")))
  
  
  (aas-set-snippets 'org-mode
                    :cond (lambda ()
                            (not (texmathp)))
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
                            (add-hook 'yas/after-exit-snippet-hook 'my/change-lang-in-snippet 0 t)
                            )
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
                    "'fg" (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "figure")))
                    "эап" (lambda ()
                            (interactive)
                            (shell-command-to-string "xdotool key Mode_switch")
                            (yas-expand-snippet (yas-lookup-snippet "figure")))
                    "'im" (lambda ()
                            (interactive)
                            (my/insert-image))
                    "эшь" (lambda ()
                            (interactive)
                            (my/insert-image))))

(provide 'init-snippets)
