;; -*- lexical-binding: t -*-

(require 'warnings)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

  (add-hook 'yas-after-exit-snippet-hook (lambda ()
                                           (interactive)
                                           (if (looking-back " \\\\)" 10)
                                               (replace-match "\\\\)"))))

  (defun my/delete-one-blank-line ()
    (interactive)
    (save-excursion
      (if (re-search-forward "^[[:space:]]*$" nil t)
          (kill-whole-line)))))

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :bind ((:map evil-insert-state-map
               ("<escape>" . evil-undo)))
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
    (my/common-snippets 'LaTeX-mode)
    (or mode (setq mode nil))
    (if (eq mode 'phys) (my/phys-snippets))
    (if (eq mode 'math) (my/math-snippets)))

  (add-hook 'aas-pre-snippet-expand-hook 'undo-boundary)

  
  (defun my/common-snippets (mode)
    (aas-set-snippets mode
                      :cond #'texmathp
                      "*"     "\\cdot "
                      "'8"    "\\ast "
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
                      ">~"    "\\gtrsim "
                      ";="    "\\neq "
                      "pm"    "\\pm "
                      "mp"    "\\mp "
                      "~~"    "\\sim "
                      "~="    "\\approx "
                      "=~"    "\\cong "
                      "~-"    "\\simeq "
                      "oxx"   "\\otimes "
                      "opl"   "\\oplus "
                      "bxx"   "\\botimes "
                      "bpl"   "\\boplus "
                      "bul"   "\\bullet "
                      ";a"    "\\alpha "
                      ";b"    "\\beta "
                      ";g"    "\\gamma "
                      ";G"    "\\Gamma "
                      ";d"    "\\delta "
                      ";D"    "\\Delta "
                      ";e"    "\\vepsilon "
                      ";E"    "\\Vepsilon "
                      ";h"    "\\hbar "
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
                      "actg"  "\\arcctg "
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
                      "lla"   "\\lla "
                      "rra"   "\\rra "
                      "div"   "\\div "
                      "prt"   "\\prt "
                      "sr"    "^2"
                      "cb"    "^3"
                      "inv"   "^{-1}"
                      "NN"    "\\N "
                      "ZZ"    "\\Z "
                      "QQ"    "\\Q "
                      "RR"    "\\R "
                      "CC"    "\\C "
                      "KK"    "\\K "
                      "inn"   "\\in "
                      "notin" "\\notin "
                      "sup"   "\\sup "
                      "Sup"   "\\supp "
                      "inf"   "\\inf "
                      "cc"    "\\subset "
                      "c=="   "\\subseteq "
                      "grad"  "\\grad "
                      "nabla" "\\nabla "
                      "Div"   "\\Div "
                      "rot"   "\\rot "
                      "Tr"    "\\Tr "
                      ";+"    "\\dagger "
                      "Im"    "\\Ima "
                      "Re"    "\\Rea "
                      "det"   "\\det "
                      "ld"    "\\ddots "
                      )
  
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
                                (yas-expand-snippet "\\\\{$1\\\\}$0"))
                      ";["    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\\\[$1\\\\]$0"))
                      ";("    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\l("))
                      ";)"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\r)"))
                      ";]"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\\\]"))
                      ";}"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\\\}$0"))
                      ";<"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\<$1\\>"))
                      ";>"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\>"))
                      "'<"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\<$1 \\mid $2\\>$0")))
  
    (aas-set-snippets mode
                      :cond #'texmathp
                      "^"     (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\hat{$1}$0"))
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
                      "sh"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\sh ${1:\\alpha }")
                                (add-hook 'post-command-hook #'my/temp-abort-snippet))
                      "ch"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\ch ${1:\\alpha }")
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
                                (yas-expand-snippet "\\mscr{$1}$0"))
                      "mrk"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\mfr{$1}$0"))
                      ".t"    (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\dot{$1}$0"))
                      "..t"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\ddot{$1}$0"))
                      "oln"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\oline{$1}$0"))
                      "uln"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\uline{$1}$0"))
                      "tld"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\tld{$1}$0"))
                      "set"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\set{$1}{$2}$0"))))
  
  
  (defun my/math-snippets ()
    (aas-set-snippets 'org-mode
                      :cond #'texmathp
                      ";\\"   "\\setminus "
                      ";->"   "\\mapsto "
                      "-<"    "\\prec "
                      ">-"    "\\succ "
                      "=="    "\\equiv "
                      "def"   "\\eqdef "
                      "ml"    "\\models "
                      "cir"   "\\circ "
                      "sgn"   "\\sgn "
                      "nii"   "\\ni "
                      "cup"   "\\cup "
                      "cap"   "\\cap "
                      "cc"    "\\subset "
                      ";cc"   "\\supset "
                      "c=="   "\\subseteq "
                      ";c=="  "\\supseteq "
                      "AA"    "\\forall "
                      "EE"    "\\exists "
                      "nEE"   "\\nexists "
                      "land"  "\\land "
                      "lor"   "\\lor "
                      "dim"   "\\dim "
                      "ker"   "\\ker "
                      ";0"    "\\emptyset ")
  
    (aas-set-snippets 'LaTeX-mode
                      :cond #'texmathp
                      ";\\"   "\\setminus "
                      ";->"   "\\mapsto "
                      "-<"    "\\prec "
                      ">-"    "\\succ "
                      "=="    "\\equiv "
                      "def"   "\\eqdef "
                      "ml"    "\\models "
                      "cir"   "\\circ "
                      "sgn"   "\\sgn "
                      "nii"   "\\ni "
                      "cup"   "\\cup "
                      "cap"   "\\cap "
                      "cc"    "\\subset "
                      ";cc"   "\\supset "
                      "c=="   "\\subseteq "
                      ";c=="  "\\supseteq "
                      "AA"    "\\forall "
                      "EE"    "\\exists "
                      "nEE"   "\\nexists "
                      "land"  "\\land "
                      "lor"   "\\lor "
                      "dim"   "\\dim "
                      "ker"   "\\ker "
                      ";0"    "\\emptyset "
                      "mod"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\Mod{$1}$0"))
                      "rng"   (lambda ()
                                (interactive)
                                (yas-expand-snippet "${1:1},${2:2},\\ldots,${3:n}$0"))
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
                                (yas-expand-snippet "\\Bigl[$1,\\ ${2:+\\infty}\\Bigr)$0"))
                      "bcap"  (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\bcap_{${1:i=1}}^{${2:n}}$0"))
                      "bcup"  (lambda ()
                                (interactive)
                                (yas-expand-snippet "\\bcup_{${1:i=1}}^{${2:n}}$0"))))
  
  
  (defun my/phys-snippets ()
    (aas-set-snippets 'LaTeX-mode
                      :cond #'texmathp
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
                      "'F"    "Ф"))
  
  
  (aas-set-snippets 'LaTeX-mode
                    :cond (lambda ()
                            (not (texmathp)))
                    "it"  "\\item "
                    "ше"  "\\item "
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
                            (my/delete-one-blank-line))
                    "ls"  (lambda ()
                            (interactive)
                            (yas-expand-snippet (yas-lookup-snippet "list"))
                            (my/delete-one-blank-line))
                    "ды"  (lambda ()
                            (interactive)
                            (shell-command-to-string "xdotool key Mode_switch")
                            (yas-expand-snippet (yas-lookup-snippet "list"))
                            (my/delete-one-blank-line)))
  
  (aas-set-snippets 'LaTeX-mode
                    :cond #'texmathp
                    "_"     (lambda ()
                              (interactive)
                              (if (looking-back "[[:space:]]+")
                                  (replace-match ""))
                              (yas-expand-snippet "_{$1}"))
                    "dd"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\frac{\\prt $1}{\\prt ${2:x}}$0"))
                    "2dd"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\frac{\\prt^2 $1}{\\prt ${2:x}^2}$0"))
                    "DD"    (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\frac{d $1}{d ${2:x}}$0"))
                    "2DD"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\frac{d^2 $1}{d ${2:x}^2}$0"))
                    "bar"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\bar{$1}$0"))
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
                              (yas-expand-snippet "\\sum $0"))
                    "Sum"   (lambda ()
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
                              (yas-expand-snippet "\\int $0"))
                    "iint"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\iint $0"))
                    "iiint" (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\iiint $0"))
                    "Int"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\int_{${1:-\\infty}}^{${2:\\infty}}${3:f(x)}d${4:x}$0"))
                    "oin"   (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\oint $0"))
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
                              (yas-expand-snippet "\\substack{$1}$0"))
                    "bigg"  (lambda ()
                              (interactive)
                              (yas-expand-snippet "\\bigg|_{$1}^{$2}$0")))
  
  
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
                            (my/insert-image)))
  
  (aas-set-snippets 'org-mode
                    :cond (lambda ()
                            (and (not (texmathp)) (= (current-column) 0)))
                    "Опр" "_*Определение*_. ")
  
  (aas-set-snippets 'org-mode
                    :cond #'texmathp
                    "_"     (lambda ()
                              (interactive)
                              (if (looking-back "[[:space:]]+")
                                  (replace-match ""))
                              (yas-expand-snippet "_"))))

(provide 'init-snippets)
