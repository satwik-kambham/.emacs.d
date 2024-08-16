;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'catppuccin-theme)
(straight-use-package 'use-package)

;; UI Customization
(load-theme 'catppuccin :no-confirm)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(column-number-mode)
(global-display-line-numbers-mode 1)

(set-frame-font "Monaspace Neon-16" nil t)
(setq line-spacing 5)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
