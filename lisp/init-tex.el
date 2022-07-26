;; -*- lexical-binding: t -*-

(use-package auctex
  :hook ((LaTeX-mode . display-line-numbers-mode)
         (LaTeX-mode . turn-on-auto-fill)
         (LaTeX-mode . (lambda ()
                         (define-key LaTeX-mode-map "\C-j" 'nil)
                         (company-mode -1)
                         (my/latex-load-prettify-symbols))))
  :custom
  (TeX-auto-save                     t)
  (TeX-parse-self                    t)
  (TeX-PDF-mode                      t)
  (TeX-file-line-error               t)
  (TeX-source-correlate-start-server t)
  (preview-auto-cache-preamble       t)
  :config
  (setq-default TeX-master nil))

(use-package cdlatex
  :after (evil yasnippet)
  :hook ((prog-mode  . turn-on-cdlatex)
         (cdlatex-tab . my/cdlatex-in-yas-field))
  :bind ((:map cdlatex-mode-map
               ("<"     . nil)
               ("("     . nil)
               ("["     . nil)
               ("{"     . nil)
               ("|"     . nil)
               ("_"     . nil)
               ("'"     . nil)
               ("`"     . nil)
               ("$"     . nil))
         (:map evil-insert-state-map
               ("<tab>" . expand-snippet-or-cdlatex-tab)))
  :config
  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun expand-snippet-or-cdlatex-tab ()
    (interactive)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (cdlatex-tab)))

  (defun my/cdlatex-in-yas-field ()
    ;; Check if we're at the end of the Yas field
    (when-let* ((_ (overlayp yas--active-field-overlay))
                (end (overlay-end yas--active-field-overlay)))
      (if (>= (point) end)
          ;; Call yas-next-field if cdlatex can't expand here
          (let ((s (thing-at-point 'sexp)))
            (unless (and s (assoc (substring-no-properties s)
                                  cdlatex-command-alist-comb))
              (yas-next-field-or-maybe-expand)
              t))
        ;; otherwise expand and jump to the correct location
        (let (cdlatex-tab-hook minp)
          (setq minp
                (min (save-excursion (cdlatex-tab)
                                     (point))
                     (overlay-end yas--active-field-overlay)))
          (goto-char minp) t)))))

(provide 'init-tex)
