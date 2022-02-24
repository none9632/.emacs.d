;; -*- lexical-binding: t -*-

(require 'init-evil)
(require 'init-basic)

(use-package org
  :defer t
  :after yasnippet
  :hook (org-mode . (lambda ()
                      (turn-on-auto-fill)
                      (variable-pitch-mode 1)
                      (visual-line-mode t)
                      (diff-hl-mode 0)))
  :bind (:map org-mode-map
              ("<tab>" . nil))
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-ellipsis                      " ▾ "
        org-src-fontify-natively          t
        org-src-tab-acts-natively         t
        org-edit-src-content-indentation  2
        org-hide-block-startup            nil
        org-src-preserve-indentation      nil
        org-cycle-separator-lines         2
        org-startup-folded                'all
        org-src-window-setup              'current-window
        org-edit-src-persistent-message   nil
        org-return-follows-link           t
        org-startup-indented              t)

  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.15)
                  (org-level-3 . 1.10)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "SauceCodePro Nerd Font" :weight 'regular :height (cdr face)))

  ;; set local scroll-margin
  (add-hook 'org-mode-hook (lambda ()
                             (set (make-variable-buffer-local 'scroll-margin) 0)))

  (use-package visual-fill-column
    :hook (org-mode . visual-fill-column-mode)
    :config
    (setq-default visual-fill-column-width       110
                  visual-fill-column-center-text t))

(use-package worf
  :after evil
  :bind ((:map evil-normal-state-map
               ("SPC t"           . worf-goto))
         (:map worf-mode-map
               ("<S-iso-lefttab>" . nil)))
  :hook (org-mode . worf-mode))

(setq org-latex-toc-command    "\\tableofcontents \\clearpage"
      org-format-latex-options (plist-put org-format-latex-options :scale 1.6)
      org-latex-create-formula-image-program 'imagemagick)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
                  [NO-DEFAULT-PACKAGES]
                  [PACKAGES]
                  [EXTRA]"
                 ("\\part{%s}"          . "\\part*{%s}")
                 ("\\section{%s}"       . "\\section*{%s}")
                 ("\\subsection{%s}"    . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}"     . "\\paragraph*{%s}"))))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode)
  :config
  (setq org-fragtog-preview-delay 0.25))

(defun my/update-theorem-and-lemma-counts ()
  (interactive)
  (setq latex-theorem-count 1
        latex-lemma-count   1)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\begin{theorem}" nil t)
      (setq latex-theorem-count (1+ latex-theorem-count)))
    (goto-char (point-min))
    (while (re-search-forward "\\\\begin{lemma}" nil t)
      (setq latex-lemma-count (1+ latex-lemma-count)))))

(defun my/inkscape-figures-create ()
  (interactive)
  (setq img-file-path (shell-command-to-string "inkscape-figures create"))
  (if (not (equal img-file-path ""))
      (progn
        (insert img-file-path)
        (if (not (equal org-inline-image-overlays nil))
            (org-toggle-inline-images))
        (org-toggle-inline-images))
    (progn
      (evil-previous-line)
      (kill-whole-line 2))))

(defun my/insert-image ()
  (interactive)
  (setq old-default-directory default-directory
        default-directory     "~/Pictures/screenshots/"
        selected-file         (ivy-read "Choose file: " #'read-file-name-internal
                                        :unwind (lambda ()
                                                  (setq default-directory old-default-directory)))
        img-file-path         (shell-command-to-string (concat "inkscape-figures move " selected-file)))
  (insert (concat "[[" img-file-path "]]"))
  (if (not (equal org-inline-image-overlays nil))
      (org-toggle-inline-images))
  (org-toggle-inline-images))

(defun my/remove-images ()
  (interactive)
  (message "Removing unused images...")
  (setq used-file-names-str "unused_name")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\./images/.+\\]\\]" nil t)
      (setq used-file-names-str (concat used-file-names-str
                                        "\\|"
                                        (replace-regexp-in-string "\\[\\|\\]"
                                                                  ""
                                                                  (match-string-no-properties 0))))))
  (shell-command-to-string (concat "find ./images/ -type f | grep -v \"" used-file-names-str "\" | xargs rm"))
  (message "Removing unused images...done"))

(defun my/inkscape-figures-edit (line-str)
  (interactive)
  (setq file-name (replace-regexp-in-string "\\[\\|\\]" "" line-str))
  (shell-command-to-string (concat "inkscape-figures edit " file-name))
  (org-toggle-inline-images)
  (org-toggle-inline-images))

(defun my/change-environment ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (save-excursion (re-search-forward "align" nil t))
        (replace-regexp "align" "gather")
      (replace-regexp "gather" "align"))))

(add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))

(defun my/org-latex-export ()
  (interactive)
  (setq tex-file-name (org-latex-export-to-latex))
  (shell-command-to-string (concat
                            "sd '^\\\\begin\\{theorem\\}' '\\\\begin{boxtheorem}' ./" tex-file-name ";"
                            "sd '^\\\\end\\{theorem\\}' '\\\\end{boxtheorem}' ./" tex-file-name ";"
                            "sd '^\\\\begin\\{lemma\\}' '\\\\begin{boxlemma}' ./" tex-file-name ";"
                            "sd '^\\\\end\\{lemma\\}' '\\\\end{boxlemma}' ./" tex-file-name ";"))
  (setq pdf-file-name (org-latex-compile tex-file-name))
  (async-shell-command (concat "zathura " pdf-file-name) nil nil))

(defun my/org-latex-mode ()
  (interactive)
  (setq org-src-window-setup      'split-window-below
        company-box-enable-icon   nil)
  (aas-activate-for-major-mode)
  (my/update-theorem-and-lemma-counts)
  (my/org-load-prettify-symbols)
  (add-hook 'org-pre-cycle-hook
            (lambda (arg)
              (cond ((eq arg 'children) (org-latex-preview nil))
                    ((eq arg 'subtree)  (progn
                                          (org-narrow-to-subtree)
                                          (save-excursion
                                            (goto-char (point-min))
                                            (while (re-search-forward "^\\*+" nil t)
                                              (org-latex-preview nil)))
                                          (widen)))
                    ((eq arg 'folded)   (if (my/line-looking-at "^\\*+[[:ascii:]]*")
                                            (progn
                                              (org-narrow-to-subtree)
                                              (save-excursion
                                                (goto-char (point-min))
                                                (while (re-search-forward "^\\*+" nil t)
                                                  (org-latex-preview '(4))))
                                              (widen))))))))

(setq org-jump-to-previous-block nil
      org-latex-mode             nil)

(defun my/isearch-line-forward (regexp-p)
  (catch 'my-catch
    (narrow-to-region (line-beginning-position) (line-end-position))
    (if (search-forward regexp-p nil t nil)
        (progn
          (widen)
          (throw 'my-catch t)))
    (widen)))

(defun my/just-one-space ()
  (insert "x")
  (if (my/isearch-line-forward "\\(")
      (progn
        (evil-backward-char 2)
        (delete-horizontal-space)))
  (backward-delete-char 1))

(defun my/org-edit-special ()
  (interactive)
  (setq line-str           (buffer-substring (line-beginning-position) (line-end-position))
        processed-line-str (replace-regexp-in-string "\\[\\[[[:word:]\\|\\.\\|/]*\\]\\]" "" line-str)
        current-layout     (shell-command-to-string "xkb-switch -p"))
  (shell-command-to-string "xdotool key Mode_switch")
  (if (and (equal processed-line-str "")
           (not (equal line-str "")))
      (my/inkscape-figures-edit line-str)
    (progn
      (org-edit-special)
      (setq-local org-latex-mode t)
      (if (equal current-layout "ru\n")
          (setq change-lang t)
        (setq change-lang nil)))))

(defun my/org-edit-src-exit ()
  (interactive)
  (yas-exit-all-snippets)
  (if (equal (count-lines (point-min) (point-max)) 1)
      (setq latex-fragment t)
    (setq latex-fragment nil))
  (org-edit-src-exit)
  (cond (change-lang                (shell-command-to-string "xkb-switch -n")))
  (cond (org-jump-to-previous-block (org-previous-block 1))
        (latex-fragment             (progn
                                      (if (org-in-item-p) (my/just-one-space))
                                      (my/isearch-line-forward "\\)")
                                      (org-latex-preview)))))

(evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'evil-window-up)
(evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'evil-window-down)
(evil-define-key '(normal insert)        org-mode-map (kbd "M-f") 'org-footnote-action)
(evil-define-key '(insert)               org-mode-map (kbd "C-i") (lambda ()
                                                                    (interactive)
                                                                    (if (org-in-item-p)
                                                                        (org-insert-item)
                                                                      (org-insert-heading))))

(leader-key-def
  "i"  'my/org-edit-special
  "m"  'org-mark-ring-goto
  "u"  (lambda ()
         (interactive)
         (if (org-at-heading-p)
             (outline-up-heading 1)
           (org-previous-visible-heading 1)))
  "d"  'org-next-visible-heading
  "q"  (lambda ()
         (interactive)
         (cond (org-latex-mode (my/org-edit-src-exit))
               (t              (evil-quit))))
  "ob" 'org-babel-tangle
  "op" 'org-latex-preview
  "oe" 'my/org-latex-export
  "ce" 'my/change-environment
  "rc" 'my/update-theorem-and-lemma-counts)

(setq org-babel-tangle-async-mode nil)

(defun my/org-babel-tangle-async-mode (&optional arg)
  (interactive)
  (if (eq arg nil)
      (cond (org-babel-tangle-async-mode (setq org-babel-tangle-async-mode nil))
            (t                           (setq org-babel-tangle-async-mode t)))
    (setq org-babel-tangle-async-mode arg)))

(defun my/org-babel-tangle-async (file)
  "Invoke `org-babel-tangle-file' asynchronously."
  (message "Tangling %s..." (buffer-file-name))
  (async-start
   (let ((args (list file)))
     `(lambda ()
        (require 'org)
        (let ((start-time (current-time)))
          (apply #'org-babel-tangle-file ',args)
          (format "%.2f" (float-time (time-since start-time))))))
   (let ((message-string (format "Tangling %S completed after " file)))
     `(lambda (tangle-time)
        (message (concat ,message-string
                         (format "%s seconds" tangle-time)))))))

(defun my/org-babel-tangle-current-buffer-async ()
  "Tangle current buffer asynchronously."
  (interactive)
  (if org-babel-tangle-async-mode
      (my/org-babel-tangle-async (buffer-file-name))))

(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'after-save-hook #'my/org-babel-tangle-current-buffer-async
                                     'run-at-end 'only-in-org-mode)))

(defun my/org-config-mode ()
  (interactive)
  (setq org-babel-tangle-async-mode       t
        org-jump-to-previous-block        t
        org-edit-src-auto-save-idle-delay 1))

)

(provide 'init-org)
