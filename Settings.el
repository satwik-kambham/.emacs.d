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

(straight-use-package 'use-package)
(straight-use-package 'catppuccin-theme)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(load-theme 'catppuccin :no-confirm)
(catppuccin-set-color 'crust "#141617")
(catppuccin-set-color 'mantle "#1d2021")
(catppuccin-set-color 'base "#1d2021")
(catppuccin-set-color 'surface0 "#282828")
(catppuccin-set-color 'surface1 "#3c3836")
(catppuccin-set-color 'surface2 "#3c3836")
(catppuccin-reload)

(global-display-line-numbers-mode 1)
(column-number-mode)
(dolist (mode '(term-mode-hook
	      eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (set-frame-font "Monaspace Neon-16" nil t)
(add-to-list 'default-frame-alist '(font . "Monaspace Neon-16"))
(setq line-spacing 5)

(use-package doom-modeline
  :straight t
  :ensure t
  :init (doom-modeline-mode 1))

(use-package dashboard
  :straight t
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-project-backend 'projectile)
  (setq dashboard-items '((recents . 5)
			  (projects . 5)))
  (setq dashboard-item-shortcuts '((recents . "r")
				   (projects . "p"))))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(use-package which-key
  :straight t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy
  :straight t
  :demand t
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1))

(use-package swiper
  :straight t
  :bind (("C-s" . swiper)))

(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)))

(use-package ivy-rich
  :straight t
  :init
  (ivy-rich-mode 1))

(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(straight-use-package 'rg)

(straight-use-package 'ryo-modal)
(straight-use-package 'expand-region)

(defun modal-set-mark-here ()
  "Set the mark at the location of the point."
  (interactive) (set-mark (point)))

(defun modal-set-mark-if-inactive ()
  "Set the mark if it isn't active."
  (interactive)
  (unless (use-region-p) (set-mark (point))))

(defun exit-modal-mode ()
  "Exit ryo modal mode."
  (interactive)
  (ryo-modal-mode 0))

(defun modal-deactivate-mark ()
  "Deactivate the mark."
  (interactive)
  (deactivate-mark))

(defun modal-select-line (count)
  "Select and expend lines."
  (interactive "p")
  (beginning-of-line)
  (unless (use-region-p) (set-mark (point)))
  (forward-line count))

(defun modal-create-new-line (count)
  "Create new line below."
  (interactive "p")
  (end-of-line)
  (dotimes (_ count)
    (electric-newline-and-maybe-indent)))

(defun modal-kill-selected-text ()
  "Kill selected text."
  (interactive)
  (kill-region (region-beginning) (region-end)))

(defun setup-modal-keybinds ()
  "Setup keybinds in navigation mode."
  (global-subword-mode t)
  (ryo-modal-keys
   ("h" backward-char :first '(modal-deactivate-mark))
   ("H" backward-char :first '(modal-set-mark-if-inactive))
   ("j" next-line :first '(modal-deactivate-mark))
   ("J" next-line :first '(modal-set-mark-if-inactive))
   ("k" previous-line :first '(modal-deactivate-mark))
   ("K" previous-line :first '(modal-set-mark-if-inactive))
   ("l" forward-char :first '(modal-deactivate-mark))
   ("L" forward-char :first '(modal-set-mark-if-inactive))
   ("g" beginning-of-buffer :first '(modal-deactivate-mark))
   ("G" end-of-buffer :first '(modal-deactivate-mark))
   ("a" forward-char :exit t)
   ("w" forward-word :first '(modal-set-mark-here))
   ("W" forward-word :first '(modal-set-mark-if-inactive))
   ("b" backward-word :first '(modal-set-mark-here))
   ("B" backward-word :first '(modal-set-mark-if-inactive))
   ("x" modal-select-line)
   ("y" kill-ring-save)
   ("p" yank)
   ("d" modal-kill-selected-text)
   ("c" modal-kill-selected-text :exit t)
   ("i" exit-modal-mode)
   ("a" forward-char :exit t)
   ("o" modal-create-new-line :exit t)))

;; (use-package ryo-modal
;;   :straight t
;;   :commands ryo-modal-mode
;;   :bind ("C-z" . ryo-modal-mode)
;;   :hook (after-init . modal-mode-setup)
;;   :config
;;   (defun modal-mode-setup ()
;;     "Setup modal mode"
;;     (setq ryo-modal-cursor-color "#cba6f8")
;;     (global-set-key (kbd "<escape>") 'ryo-modal-mode)
;;     (setup-modal-keybinds)))
(use-package evil
  :straight t
  :ensure t
  :config
  (evil-mode 1))

(straight-use-package 'auto-complete)

(use-package magit
  :straight t)

(use-package envrc
  :straight t
  :hook (after-init . envrc-global-mode))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))
