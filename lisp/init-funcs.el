;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'init-custom)

(defun my/open-org-file ()
  "Open `.emacs.d.org'."
  (interactive)
  (setq org-file (expand-file-name "emacs.d.org" user-emacs-directory))
  (message "Opening ~/.emacs.d/.emacs.d.org...")
  (find-file org-file)
  (message "Opening ~/.emacs.d/.emacs.d.org...done"))

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun centaur-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

Save to `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value))

(defun my/line-looking-at (regexp)
  (save-excursion
    (beginning-of-line)
    (looking-at-p regexp)))

(defun my/current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[[:space:]]*$")))

;; (defun my/test ()
;;   (interactive)
;;   (if (my/line-looking-at "^\\*+[[:ascii:]]*")
;; 	  (message "test")))

(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package archives (ELPA).

REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern (completing-read "Select package archives: "
                             (mapcar #'car centaur-package-archives-alist)))))
  ;; Set option
  (centaur-set-variable 'centaur-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))
(defalias 'centaur-set-package-archives #'set-package-archives)

(defvar centaur--updating-packages nil)
(defun update-packages (&optional sync)
  "Refresh package contents and update all packages.

If SYNC is non-nil, the updating process is synchronous."
  (interactive)
  (when centaur--updating-packages
    (user-error "Still updating packages..."))

  (message "Updating packages...")
  (if (and (not sync)
           (require 'async nil t))
      (progn
        (setq centaur--updating-packages t)
        (async-start
         `(lambda ()
            ,(async-inject-variables "\\`\\(load-path\\)\\'")
            (require 'init-funcs)
            (require 'init-package)
            (upgrade-packages)
            (with-current-buffer auto-package-update-buffer-name
              (buffer-string)))
         (lambda (result)
           (setq centaur--updating-packages nil)
           (message "%s" result)
           (message "Updating packages...done"))))
    (progn
      (upgrade-packages)
      (message "Updating packages...done"))))
(defalias 'centaur-update-packages #'update-packages)

(provide 'init-funcs)
