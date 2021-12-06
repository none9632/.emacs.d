;; -*- lexical-binding: t no-byte-compile: t -*-

(defvar centaur-gc-cons-threshold 16000000
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar centaur-gc-cons-upper-limit 400000000
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar centaur-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold centaur-gc-cons-upper-limit
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold centaur-gc-cons-threshold
                  gc-cons-percentage 0.1)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my/minibuffer-setup-hook ()
              (setq gc-cons-threshold centaur-gc-cons-upper-limit))

            (defun my/minibuffer-exit-hook ()
              (setq gc-cons-threshold centaur-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)))

;; Hide cursor and mode-line at startup
(setq-default mode-line-format nil)
(internal-show-cursor nil nil)

;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(advice-add #'package-initialize :after #'update-load-path)

(update-load-path)

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

(require 'init-dashboard)
;; (require 'init-dired)
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
;; (require 'init-c)
;; (require 'init-python)
