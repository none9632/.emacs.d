;; -*- lexical-binding: t -*-

(require 'init-evil)
(require 'init-basic)

(use-package org
  :defer t
  :after yasnippet
  :hook (org-mode . (lambda ()
                      (turn-on-auto-fill)
                      (variable-pitch-mode 1)
                      (diff-hl-mode 0)))
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-ellipsis                      " ▾ "
        org-src-fontify-natively          t
        org-src-tab-acts-natively         t
        org-edit-src-content-indentation  2
        org-hide-block-startup            nil
        org-src-preserve-indentation      nil
        org-cycle-separator-lines         2
        org-startup-folded                t
        org-src-window-setup              'current-window
        org-edit-src-persistent-message   nil
        org-return-follows-link           t
        org-startup-indented              t
        org-startup-with-inline-images    nil)

  (setcar (nthcdr 4 org-emphasis-regexp-components) 10)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

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

  (use-package org-auto-tangle
    :hook (org-mode . org-auto-tangle-mode))

(use-package worf
  :after evil
  :bind ((:map evil-normal-state-map
               ("SPC t"           . worf-goto))
         (:map worf-mode-map
               ("<S-iso-lefttab>" . nil)
               ("["               . nil)
               ("]"               . nil)))
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
  (async-shell-command (concat "evince " pdf-file-name) nil nil))

(defun my/get-org-latex-fragment-image ()
  (interactive)
  (catch 'my-catch
    (let* ((processing-info
            (cdr (assq org-preview-latex-default-process org-preview-latex-process-alist)))
           (face (face-at-point))
           ;; Get the colors from the face at point.
           (fg
            (let ((color (plist-get org-format-latex-options
                                    :foreground)))
              (if 'forbuffer
                  (cond
                   ((eq color 'auto)
                    (face-attribute face :foreground nil 'default))
                   ((eq color 'default)
                    (face-attribute 'default :foreground nil))
                   (t color))
                color)))
           (bg
            (let ((color (plist-get org-format-latex-options
                                    :background)))
              (if 'forbuffer
                  (cond
                   ((eq color 'auto)
                    (face-attribute face :background nil 'default))
                   ((eq color 'default)
                    (face-attribute 'default :background nil))
                   (t color))
                color)))
           (value (org-element-property :value (org-element-context)))
           (hash (sha1 (prin1-to-string
                        (list org-format-latex-header
                              org-latex-default-packages-alist
                              org-latex-packages-alist
                              org-format-latex-options
                              'forbuffer value fg bg))))
           (imagetype (or (plist-get processing-info :image-output-type) "png"))
           (prefix (concat org-preview-latex-image-directory "org-ltximg"))
           (absprefix (expand-file-name prefix default-directory))
           (movefile (format "%s_%s.%s" absprefix hash imagetype)))
      (throw 'my-catch movefile))))

(defun my/view-org-fragment ()
  (interactive)
  (unless (bound-and-true-p my/latex-window-frame)
    (async-start
     (progn
       (setq latex-fragment-buffer (get-buffer-create " *latex-fragment*"))
       (let ((latex-image-file   nil)
             (latex-image        nil)
             (latex-image-width  nil)
             (latex-image-height nil))
         (save-excursion
           (forward-char)
           (re-search-backward "(")
           (re-search-forward  "[[:digit:]]+")
           (re-search-backward (concat "\\\\tag{" (match-string-no-properties 0) "}"))
           (setq latex-image-file   (my/get-org-latex-fragment-image)
                 latex-image        (create-image latex-image-file)
                 latex-image-width  (car (image-size latex-image t))
                 latex-image-height (cdr (image-size latex-image t))))
         (with-current-buffer latex-fragment-buffer
           (insert "\n   ")
           (insert-image latex-image))
         (if (or (eq latex-image-height 85)
                 (eq latex-image-height 31))
             (setq latex-image-height (+ latex-image-height 10)))
         (setq my/latex-window-frame (posframe-show
                                      latex-fragment-buffer
                                      :position (point)
                                      :font "SauceCodePro Nerd Font 6"
                                      :width (floor (* latex-image-width 0.21))
                                      :height (+ 4 (floor (/ (- latex-image-height 22) 10.55)))
                                      :border-width 2
                                      :border-color "#32424b"
                                      :override-parameters '((parent-frame . nil)))))))
    (add-hook 'post-command-hook #'my/hide-org-fragment)))

(defun my/hide-org-fragment ()
  (interactive)
  (let ((current-postiton (point)))
    (unless (equal current-postiton my/previous-position)
      (if my/latex-window-frame
          (progn
            (posframe-delete-frame latex-fragment-buffer)
            (setq my/latex-window-frame nil)
            (evil-delete-buffer latex-fragment-buffer)
            (remove-hook 'post-command-hook #'my/hide-org-fragment))))))

(defun my/org-latex-mode ()
  (interactive)
  (setq-local org-src-window-setup    'split-window-below
              company-box-enable-icon nil
              org-latex-mode          t)
  (aas-activate-for-major-mode)
  (visual-line-mode t)
  (my/update-theorem-and-lemma-counts)
  (my/org-load-prettify-symbols)
  (my/remove-images)
  (add-hook 'org-pre-cycle-hook
            (lambda (arg)
              (cond ((eq arg 'children) (progn
                                          (org-narrow-to-subtree)
                                          (save-excursion
                                            (goto-char (point-min))
                                            (search-forward-regexp "^\*+ " nil t 2)
                                            (org-display-inline-images nil t (point-min) (point))
                                            (org--latex-preview-region (point-min) (point)))
                                          (widen)))
                    ((eq arg 'subtree)  (progn
                                          (org-narrow-to-subtree)
                                          (org-display-inline-images nil t (point-min) (point-max))
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
                                              (mapc #'delete-overlay (overlays-in (point-min) (point-max)))
                                              (widen))))))))

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

(defun my/inkscape-figures-edit (line-str)
  (interactive)
  (setq file-name (replace-regexp-in-string "\\[\\|\\]" "" line-str))
  (shell-command-to-string (concat "inkscape-figures edit " file-name))
  (org-toggle-inline-images)
  (org-toggle-inline-images))

(defun my/insert-image ()
  (interactive)
  (let ((image-path   (my/lf-select-file "~/Pictures/screenshots"))
        (image        nil)
        (image-width  nil)
        (image-height nil))
    (unless (equal image-path "cancel")
      (setq image        (create-image image-path)
            image-width  (car (image-size image t))
            image-height (cdr (image-size image t)))
      (if (or (> image-width 1050)
              (> image-height 600))
          (shell-command-to-string (concat "convert "
                                           image-path
                                           " -quality 100 -resize 1050x600 "
                                           image-path)))
      (setq image-path (shell-command-to-string (concat "inkscape-figures move " image-path)))
      (insert (concat "[[" image-path "]]"))
      (org-display-inline-images nil t (point-at-bol) (point-at-eol)))))

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
  (if (and (bound-and-true-p org-latex-mode)
           (looking-at "(\\|)\\|[[:digit:]]+"))
      (save-excursion
        (setq my/previous-position (point))
        (narrow-to-region (line-beginning-position) (line-end-position))
        (if (or (looking-at "(")
                (re-search-backward "(" nil t))
            (progn
              (widen)
              (if (looking-at "([[:digit:]]+)")
                  (my/view-org-fragment)))
          (widen)))
    (let* ((line-str           (buffer-substring (line-beginning-position) (line-end-position)))
           (processed-line-str (replace-regexp-in-string "\\[\\[[[:word:]\\|\\.\\|/]*\\]\\]" "" line-str))
           (current-layout     (shell-command-to-string "xkb-switch -p")))
      (shell-command-to-string "xdotool key Mode_switch")
      (if (and (equal processed-line-str "")
               (not (equal line-str "")))
          (my/inkscape-figures-edit line-str)
        (progn
          (org-edit-special)
          (toggle-truncate-lines)
          (setq-local previous-major-mode-is-org t)
          (if (equal current-layout "ru\n")
              (setq change-lang t)
            (setq change-lang nil)))))))

(defun my/org-edit-src-exit ()
  (interactive)
  (yas-exit-all-snippets)
  (let ((latex-fragment nil))
    (if (equal (count-lines (point-min) (point-max)) 1)
        (progn
          (goto-char (- (point-max) 2))
          (delete-horizontal-space)
          (setq latex-fragment t))
      (setq latex-fragment nil))
    (org-edit-src-exit)
    (cond (change-lang                                   (shell-command-to-string "xkb-switch -n")))
    (cond ((bound-and-true-p org-jump-to-previous-block) (org-previous-block 1))
          (latex-fragment                                (progn
                                                           (if (org-in-item-p) (my/just-one-space))
                                                           (my/isearch-line-forward "\\)")
                                                           (org-latex-preview))))))

(defun my/org-insert-item-or-heading ()
  (interactive)
  (if (org-in-item-p)
      (org-insert-item)
    (org-insert-heading)))

(defun my/org-previous-visible-heading ()
  (interactive)
  (if (org-at-heading-p)
      (progn
        (save-excursion
          (previous-line)
          (if (org-at-heading-p)
              (setq prev-heading-folded t)
            (setq prev-heading-folded nil)))
        (if prev-heading-folded
            (outline-up-heading 1)
          (org-previous-visible-heading 1)))
    (org-previous-visible-heading 1)))

(evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'evil-window-up)
(evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'evil-window-down)
(evil-define-key '(normal insert)        org-mode-map (kbd "M-f") 'org-footnote-action)
(evil-define-key '(insert)               org-mode-map (kbd "C-i") 'my/org-insert-item-or-heading)

(leader-key-def
  "i"   'my/org-edit-special
  "m"   'org-mark-ring-goto
  "u"   'my/org-previous-visible-heading
  "d"   'org-next-visible-heading
  "ob"  'org-babel-tangle
  "op"  'org-latex-preview
  "oe"  'my/org-latex-export
  "ce"  'my/change-environment
  "rc"  'my/update-theorem-and-lemma-counts
  "TAB" 'evil-close-folds)

(defun my/org-config-mode ()
  (interactive)
  (setq org-jump-to-previous-block        t
        org-edit-src-auto-save-idle-delay 1
        org-auto-tangle-default           t))

)

(provide 'init-org)
