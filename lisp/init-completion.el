;; -*- lexical-binding: t -*-

(require 'init-custom)
(require 'init-funcs)

(use-package company
  :diminish
  :bind (:map company-active-map
              ("<tab>"     . expand-snippet-or-complete-selection)
              ("<backtab>" . company-select-previous))
  :hook ((after-init . global-company-mode))
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit             12
        company-idle-delay                0
        company-minimum-prefix-length     2
        company-box-scrollbar             nil
        company-require-match             nil
        company-dabbrev-ignore-case       nil
        company-dabbrev-downcase          nil
        company-global-modes             '(not erc-mode message-mode help-mode
                                               gud-mode eshell-mode shell-mode))
  :config
  ;; (setq company-backends
  ;;       '((company-files          ; files & directory
  ;;          company-keywords       ; keywords
  ;;          company-capf
  ;;          company-yasnippet
  ;;          )
  ;;         (company-abbrev company-dabbrev)
  ;;         ))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun expand-snippet-or-complete-selection ()
    (interactive)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand))
            (company-abort)
            (not (org-try-cdlatex-tab)))
        (company-complete-common-or-cycle))))

(use-package company-prescient
  :after company
  :init
  (company-prescient-mode 1))

(use-package company-box
  :after company
  :diminish
  :defines company-box-icons-all-the-icons
  :hook (company-mode . company-box-mode)
  :init (setq company-box-enable-icon     centaur-icon
              company-box-backends-colors nil
              company-box-doc-enable      nil)
  :config
  (when (icons-displayable-p)
    (declare-function all-the-icons-faicon   'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon  'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.8  :v-adjust -0.15                             ))
            (Text          . ,(all-the-icons-faicon   "text-width"               :height 0.8  :v-adjust -0.02                             ))
            (Method        . ,(all-the-icons-faicon   "cube"                     :height 0.8  :v-adjust -0.02 :face 'all-the-icons-purple ))
            (Function      . ,(all-the-icons-faicon   "cube"                     :height 0.8  :v-adjust -0.02 :face 'all-the-icons-purple ))
            (Constructor   . ,(all-the-icons-faicon   "cube"                     :height 0.8  :v-adjust -0.02 :face 'all-the-icons-purple ))
            (Field         . ,(all-the-icons-octicon  "tag"                      :height 0.85 :v-adjust 0     :face 'all-the-icons-lblue  ))
            (Variable      . ,(all-the-icons-octicon  "tag"                      :height 0.85 :v-adjust 0     :face 'all-the-icons-lblue  ))
            (Class         . ,(all-the-icons-material "settings_input_component" :height 0.8  :v-adjust -0.15 :face 'all-the-icons-orange ))
            (Interface     . ,(all-the-icons-material "share"                    :height 0.8  :v-adjust -0.15 :face 'all-the-icons-lblue  ))
            (Module        . ,(all-the-icons-material "view_module"              :height 0.8  :v-adjust -0.15 :face 'all-the-icons-lblue  ))
            (Property      . ,(all-the-icons-faicon   "wrench"                   :height 0.8  :v-adjust -0.02                             ))
            (Unit          . ,(all-the-icons-material "settings_system_daydream" :height 0.8  :v-adjust -0.15                             ))
            (Value         . ,(all-the-icons-material "format_align_right"       :height 0.8  :v-adjust -0.15 :face 'all-the-icons-lblue  ))
            (Enum          . ,(all-the-icons-material "storage"                  :height 0.8  :v-adjust -0.15 :face 'all-the-icons-orange ))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.8  :v-adjust -0.15                             ))
            (Snippet       . ,(all-the-icons-material "format_align_center"      :height 0.8  :v-adjust -0.15                             ))
            (Color         . ,(all-the-icons-material "palette"                  :height 0.8  :v-adjust -0.15                             ))
            (File          . ,(all-the-icons-faicon   "file-o"                   :height 0.8  :v-adjust -0.02                             ))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.8  :v-adjust -0.15                             ))
            (Folder        . ,(all-the-icons-faicon   "folder-open"              :height 0.8  :v-adjust -0.02                             ))
            (EnumMember    . ,(all-the-icons-material "format_align_right"       :height 0.8  :v-adjust -0.15                             ))
            (Constant      . ,(all-the-icons-faicon   "square-o"                 :height 0.8  :v-adjust -0.1                              ))
            (Struct        . ,(all-the-icons-material "settings_input_component" :height 0.8  :v-adjust -0.15 :face 'all-the-icons-orange ))
            (Event         . ,(all-the-icons-octicon  "zap"                      :height 0.8  :v-adjust 0     :face 'all-the-icons-orange ))
            (Operator      . ,(all-the-icons-material "control_point"            :height 0.8  :v-adjust -0.15                             ))
            (TypeParameter . ,(all-the-icons-faicon   "arrows"                   :height 0.8  :v-adjust -0.02                             ))
            (Template      . ,(all-the-icons-material "format_align_left"        :height 0.8  :v-adjust -0.15                             )))
          company-box-icons-alist 'company-box-icons-all-the-icons)))

(provide 'init-completion)
