;; -*- lexical-binding: t -*-

(use-package auctex
  :hook ((LaTeX-mode . display-line-numbers-mode)
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . turn-on-auto-fill)
         (LaTeX-mode . (lambda ()
                         (interactive)
                         (define-key LaTeX-mode-map "\C-j" 'nil)
                         (add-hook 'find-file-hook 'TeX-fold-buffer t)
                         (electric-pair-local-mode -1)
                         (company-mode -1)
                         (my/latex-load-prettify-symbols))))
  :custom
  (TeX-auto-save                     t)
  (TeX-parse-self                    t)
  (TeX-PDF-mode                      t)
  (TeX-file-line-error               t)
  (TeX-fold-auto                     t)
  (TeX-source-correlate-start-server t)
  (preview-auto-cache-preamble       t)
  :config
  (setq-default TeX-master nil))

(use-package cdlatex
  :after yasnippet
  :hook ((org-mode    . turn-on-cdlatex)
         (LaTeX-mode  . turn-on-cdlatex)
         (cdlatex-tab . my/cdlatex-in-yas-field))
  :bind ((:map cdlatex-mode-map
               ("<"     . nil)
               ("("     . nil)
               ("["     . nil)
               ("{"     . nil)
               ("|"     . nil)
               ("_"     . nil)
               ("'"     . nil)
               ("`"     . nil))
         (:map evil-insert-state-map
               ("<tab>" . cdlatex-tab))
         (:map evil-normal-state-map
               ("<tab>" . org-cycle)))
  :config
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
