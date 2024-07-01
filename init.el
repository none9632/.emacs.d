;; -*- lexical-binding: t no-byte-compile: t -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Hide cursor and mode-line at startup
(setq-default mode-line-format nil)
(internal-show-cursor nil nil)

;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun my/update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(advice-add #'package-initialize :after #'my/update-load-path)

(my/update-load-path)

;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package)

;; Preferences
(require 'init-basic)
;; (require 'init-hydra)

(require 'init-ui)
(require 'init-evil)
(require 'init-edit)
(require 'init-ivy)
(require 'init-completion)
(require 'init-snippets)

(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-window)
(require 'init-treemacs)

;; (require 'init-eshell)
;; (require 'init-shell)

;; (require 'init-markdown)
(require 'init-org)

(require 'init-utils)

;; Programming
(require 'init-vcs)
;; (require 'init-flycheck)
(require 'init-projectile)

(require 'init-prog)
(require 'init-elisp)
(require 'init-tex)
