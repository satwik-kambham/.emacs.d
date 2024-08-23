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

(straight-use-package 'meow)

(defun meow-setup ()
(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
(meow-motion-overwrite-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 '("<escape>" . ignore))
(meow-leader-define-key
 ;; SPC j/k will run the original command in MOTION state.
 '("j" . "H-j")
 '("k" . "H-k")
 ;; Use SPC (0-9) for digit arguments.
 '("1" . meow-digit-argument)
 '("2" . meow-digit-argument)
 '("3" . meow-digit-argument)
 '("4" . meow-digit-argument)
 '("5" . meow-digit-argument)
 '("6" . meow-digit-argument)
 '("7" . meow-digit-argument)
 '("8" . meow-digit-argument)
 '("9" . meow-digit-argument)
 '("0" . meow-digit-argument)
 '("/" . meow-keypad-describe-key)
 '("?" . meow-cheatsheet))
(meow-normal-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 '("l" . meow-right)
 '("h" . meow-left)
 '("/" . meow-search)
 '("y" . meow-save)
 '("p" . meow-yank)
 '("d" . meow-kill)
 '("o" . meow-open-below)
 '("O" . meow-open-above)
 '("a" . meow-append)
 '("c" . meow-change)
 '("g" . meow-cancel-selection)
 '("i" . meow-insert)
 '("x" . meow-line)
 '("0" . meow-expand-0)
 '("9" . meow-expand-9)
 '("8" . meow-expand-8)
 '("7" . meow-expand-7)
 '("6" . meow-expand-6)
 '("5" . meow-expand-5)
 '("4" . meow-expand-4)
 '("3" . meow-expand-3)
 '("2" . meow-expand-2)
 '("1" . meow-expand-1)
 '("-" . negative-argument)
 '(";" . meow-reverse)
 '("b" . meow-back-word)
 '("B" . meow-back-symbol)
 '("e" . meow-next-word)
 '("u" . meow-undo)
 '("<escape>" . ignore)))

(require 'meow)
(meow-setup)
(meow-global-mode 1)
