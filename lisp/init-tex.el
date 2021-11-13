;; -*- lexical-binding: t -*-

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :bind (:map pdf-view-mode-map
              ;; ("SPC" . nil)
              ;; ("SPC h v" . 'counsel-describe-variable-function)
              ("j"       . pdf-view-next-line-or-next-page)
              ("k"       . pdf-view-previous-line-or-previous-page)
              ("J"       . pdf-view-next-page)
              ("K"       . pdf-view-previous-page)
              )
  :hook (pdf-view-mode . solaire-mode)
  :config
  (pdf-tools-install :no-query)

  (pdf-view-themed-minor-mode)

  ;; (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.1))

(use-package auctex
  :hook ((LaTeX-mode . display-line-numbers-mode)
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . turn-on-auto-fill)
         (LaTeX-mode . (lambda ()
                         (interactive)
                         (define-key LaTeX-mode-map "\C-j" 'nil)
                         (add-hook 'find-file-hook 'TeX-fold-buffer t)
                         (electric-pair-local-mode -1)
                         (my/latex-load-prettify-symbols))))
  :custom
  (TeX-auto-save                     t)
  (TeX-parse-self                    t)
  (TeX-PDF-mode                      t)
  (TeX-file-line-error               t)
  (TeX-fold-auto                     t)
  (TeX-source-correlate-start-server t)
  (TeX-view-program-selection        '((output-pdf "pdf-tools"))
                                     TeX-source-correlate-start-server t)
  (TeX-view-program-list             '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (preview-auto-cache-preamble       t)
  :config
  (setq-default TeX-master nil)

  (bind-key "SPC p d" #'preview-document LaTeX-mode-map))

;; (use-package evil-tex
;;   :hook (LaTeX-mode . evil-tex-mode))

(use-package cdlatex
  :after yasnippet
  ;; :hook ((LaTeX-mode  . turn-on-cdlatex)
  ;;        (cdlatex-tab . my/cdlatex-in-yas-field))
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab))
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
