;; -*- lexical-binding: t -*-

(require 'init-custom)
(require 'init-funcs)

;; (use-package persp-mode
;;   :diminish
;;   :defines (recentf-exclude ivy-ignore-buffers)
;;   :commands (get-current-persp persp-contain-buffer-p)
;;   :hook ((after-init . persp-mode)
;;          (persp-mode . persp-load-frame)
;;          (kill-emacs . persp-save-frame))
;;   :init (setq persp-keymap-prefix (kbd "C-x p")
;;               persp-nil-name "default"
;;               persp-set-last-persp-for-new-frames nil
;;               persp-kill-foreign-buffer-behaviour 'kill
;;               persp-auto-resume-time (if centaur-dashboard 0 1.0))
;;   :config
;;   ;; Save and load frame parameters (size & position)
;;   (defvar persp-frame-file (expand-file-name "persp-frame" persp-save-dir)
;;     "File of saving frame parameters.")

;;   (defun persp-save-frame ()
;;     "Save the current frame parameters to file."
;;     (interactive)
;;     (when (and (display-graphic-p) centaur-restore-frame-geometry persp-mode)
;;       (condition-case error
;;           (with-temp-buffer
;;             (erase-buffer)
;;             (insert
;;              ";;; -*- mode: emacs-lisp; coding: utf-8-unix -*-\n"
;;              ";;; This is the previous frame parameters.\n"
;;              ";;; Last generated " (current-time-string) ".\n"
;;              "(setq initial-frame-alist\n"
;;              (format "      '((top . %d)\n" (frame-parameter nil 'top))
;;              (format "        (left . %d)\n" (frame-parameter nil 'left))
;;              (format "        (width . %d)\n" (frame-parameter nil 'width))
;;              (format "        (height . %d)\n" (frame-parameter nil 'height))
;;              (format "        (fullscreen . %s)))\n" (frame-parameter nil 'fullscreen)))
;;             (when (file-writable-p persp-frame-file)
;;               (write-file persp-frame-file)))
;;         (error
;;          (warn "persp frame: %s" (error-message-string error))))))

;;   (defun persp-load-frame ()
;;     "Load frame with the previous frame's geometry."
;;     (interactive)
;;     (when (and (display-graphic-p) centaur-restore-frame-geometry persp-mode)
;;       (when (file-readable-p persp-frame-file)
;;         (condition-case error
;;             (progn
;;               (load persp-frame-file)

;;               ;; Handle multiple monitors gracefully
;;               (when (>= (eval (frame-parameter nil 'left)) (display-pixel-width))
;;                 (set-frame-parameter nil 'left 0))
;;               (when (>= (eval (frame-parameter nil 'top)) (display-pixel-height))
;;                 (set-frame-parameter nil 'top 0)))
;;           (error
;;            (warn "persp frame: %s" (error-message-string error)))))))

;;   (with-no-warnings
;;     ;; Don't save if the sate is not loaded
;;     (defvar persp-state-loaded nil
;;       "Whether the state is loaded.")

;;     (defun my-persp-after-load-state (&rest _)
;;       (setq persp-state-loaded t))
;;     (advice-add #'persp-load-state-from-file :after #'my-persp-after-load-state)
;;     (add-hook 'emacs-startup-hook
;;               (lambda ()
;;                 (add-hook 'find-file-hook #'my-persp-after-load-state)))

;;     (defun my-persp-asave-on-exit (fn &optional interactive-query)
;;       (if persp-state-loaded
;;           (funcall fn interactive-query)
;;         t))
;;     (advice-add #'persp-asave-on-exit :around #'my-persp-asave-on-exit))

;;   ;; Don't save dead or temporary buffers
;;   (add-to-list 'persp-filter-save-buffers-functions
;;                (lambda (b)
;;                  "Ignore dead buffers."
;;                  (not (buffer-live-p b))))
;;   (add-to-list 'persp-filter-save-buffers-functions
;;                (lambda (b)
;;                  "Ignore temporary buffers."
;;                  (let ((bname (file-name-nondirectory (buffer-name b))))
;;                    (or (string-prefix-p ".newsrc" bname)
;;                        (string-prefix-p "magit" bname)
;;                        (string-prefix-p "Pfuture-Callback" bname)
;;                        (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
;;                        (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)
;;                        (eq (buffer-local-value 'major-mode b) 'erc-mode)
;;                        (eq (buffer-local-value 'major-mode b) 'rcirc-mode)
;;                        (eq (buffer-local-value 'major-mode b) 'nov-mode)
;;                        (eq (buffer-local-value 'major-mode b) 'vterm-mode)))))

;;   ;; Don't save persp configs in `recentf'
;;   (with-eval-after-load 'recentf
;;     (push persp-save-dir recentf-exclude))

;;   ;; Ivy Integraticon
;;   (with-eval-after-load 'ivy
;;     (add-to-list 'ivy-ignore-buffers
;;                  #'(lambda (b)
;;                      (when persp-mode
;;                        (let ((persp (get-current-persp)))
;;                          (if persp
;;                              (not (persp-contain-buffer-p b persp))
;;                            nil))))))

;;   ;; Eshell integration
;;   (persp-def-buffer-save/load
;;    :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
;;    :save-vars '(major-mode default-directory))

;;   ;; Shell integration
;;   (persp-def-buffer-save/load
;;    :mode 'shell-mode :tag-symbol 'def-shell-buffer
;;    :mode-restore-function (lambda (_) (shell))
;;    :save-vars '(major-mode default-directory)))

;; (use-package persp-mode-projectile-bridge
;;   :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
;;              persp-mode-projectile-bridge-kill-perspectives)
;;   :hook ((persp-mode . persp-mode-projectile-bridge-mode)
;;          (persp-mode-projectile-bridge-mode
;;           .
;;           (lambda ()
;;             (if persp-mode-projectile-bridge-mode
;;                 (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
;;               (persp-mode-projectile-bridge-kill-perspectives)))))
;;   :init (setq persp-mode-projectile-bridge-persp-name-prefix "[p]")
;;   :config
;;   (with-no-warnings
;;     ;; HACK: Allow saving to files
;;     (defun my-persp-mode-projectile-bridge-add-new-persp (name)
;;       (let ((persp (persp-get-by-name name *persp-hash* :nil)))
;;         (if (eq :nil persp)
;;             (prog1
;;                 (setq persp (persp-add-new name))
;;               (when persp
;;                 (set-persp-parameter 'persp-mode-projectile-bridge t persp)
;;                 (persp-add-buffer (projectile-project-buffers)
;;                                   persp nil nil)))
;;           persp)))
;;     (advice-add #'persp-mode-projectile-bridge-add-new-persp
;;                 :override #'my-persp-mode-projectile-bridge-add-new-persp)

;;     ;; HACK: Switch to buffer after switching perspective
;;     (defun my-persp-mode-projectile-bridge-hook-switch (&rest _args)
;;       (let* ((buf (current-buffer))
;;              (persp (persp-mode-projectile-bridge-find-perspective-for-buffer buf)))
;;         (when persp
;;           (when (buffer-live-p
;;                  persp-mode-projectile-bridge-before-switch-selected-window-buffer)
;;             (let ((win (selected-window)))
;;               (unless (eq (window-buffer win)
;;                           persp-mode-projectile-bridge-before-switch-selected-window-buffer)
;;                 (set-window-buffer
;;                  win persp-mode-projectile-bridge-before-switch-selected-window-buffer)
;;                 (setq persp-mode-projectile-bridge-before-switch-selected-window-buffer nil))))
;;           (persp-frame-switch (persp-name persp))

;;           (when (buffer-live-p buf)
;;             (switch-to-buffer buf)))))
;;     (advice-add #'persp-mode-projectile-bridge-hook-switch
;;                 :override #'my-persp-mode-projectile-bridge-hook-switch)))

(provide 'init-persp)
