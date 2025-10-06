;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp")

(dolist (mode '(menu-bar-mode scroll-bar-mode tool-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq ring-bell-function 'ignore
      inhibit-startup-message t
      gc-cons-threshold 100000000
      indent-tabs-mode nil
      case-fold-search nil
      show-paren-delay 0
      dabbrev-case-fold-search nil
      dired-dwim-target t
      eldoc-echo-area-use-multiline-p nil)

(dolist (mode '(savehist-mode
                column-number-mode
                save-place-mode
                show-paren-mode
                recentf-mode))
  (funcall mode 1))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(setq recentf-max-saved-items 1000
      recentf-max-menu-items 1000)

(require 'wsl)

(require 'lisp-indent-function)
(setq lisp-indent-function #'Fuco1/lisp-indent-function)

(add-to-list 'elisp-flymake-byte-compile-load-path "~/.emacs.d/lisp")
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t)

(use-package diminish
  :demand t
  :config
  (diminish 'eldoc-mode)
  (with-eval-after-load 'autorevert
    (diminish 'auto-revert-mode)))

(use-package general
  :demand t
  :config
  (general-evil-setup)

  (general-create-definer general-spc
    :states 'normal
    :keymaps 'override
    :prefix "SPC")

  (general-create-definer general-comma
    :keymaps 'override
    :states 'normal
    :prefix ",")

  (general-spc
    "u" #'universal-argument
    "-" #'negative-argument
    "p" project-prefix-map
    "e" #'display-local-help
	"f f" #'find-file)

  (general-def normal
    "-" #'dired-jump
    "_" #'dired-jump-other-window)

  (general-def "C-h M-k" #'describe-keymap)

  (general-comma
    "b b" #'switch-to-buffer
    "b k" #'kill-buffer)

  (general-def normal emacs-lisp-mode-map
    "K" #'describe-symbol)

  (general-def normal (emacs-lisp-mode-map lisp-interaction-mode-map)
    ", f" #'eval-defun
    ", k" #'eval-buffer)

  (general-def (normal insert) lisp-interaction-mode-map
    "C-j" #'eval-print-last-sexp))

(use-package evil
  :demand t
  :general
  (general-comma "C" #'hy/bind-command)
  (general-def (evil-normal-state-map evil-visual-state-map) "u" #'undo)
  (general-def normal (emacs-lisp-mode-map lisp-interaction-mode-map)
    ", e" #'hy/evil-eval)
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-respect-visual-line-mode t
        evil-search-module 'evil-search
        evil-symbol-word-search t
		evil-split-window-below t
		evil-vsplit-window-right t)
  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)

  (evil-define-operator hy/evil-eval (beg end type)
    :move-point nil
    (eval-region beg end t))

  (defun hy/nohighlight ()
    (when (memq this-command evil-motions)
      (evil-ex-nohighlight)))

  (add-hook 'post-command-hook #'hy/nohighlight)

  (defun hy/bind-command (command key)
    (interactive "sShell command: \nKKey: ")
    (let ((f (lambda ()
               (interactive)
               (shell-command command))))
      (define-key evil-normal-state-map key f)))

  (advice-add 'evil-visual-update-x-selection :override #'ignore)
  (evil-mode))

(use-package evil-collection
  :demand t
  :general
  (general-def normal evil-collection-unimpaired-mode-map
    "[d" #'evil-collection-unimpaired-previous-error
    "]d" #'evil-collection-unimpaired-next-error
    "[q" nil "]q" nil)
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package magit
  :general
  (general-spc "g g" #'magit-status)
  :init
  (setq magit-bury-buffer-function 'magit-mode-quit-window)
  (with-eval-after-load 'project
    (define-key project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))
  (evil-ex-define-cmd "G" #'magit-status))

(defconst hy/lisp-mode-hooks
  '(emacs-lisp-mode-hook
    clojure-mode-hook
    cider-repl-mode-hook
    inferior-lisp-mode-hook
    eval-expression-minibuffer-setup-hook
    lisp-mode-hook
    sly-mrepl-mode-hook))

(use-package paredit
  :ghook hy/lisp-mode-hooks
  :general
  (general-def normal paredit-mode-map
    ", (" #'paredit-wrap-round
    ", [" #'paredit-wrap-square
    ", {" #'paredit-wrap-curly
    ", O" #'paredit-raise-sexp
    ", @" #'paredit-splice-sexp)
  (general-def paredit-mode-map
	"RET" #'paredit-newline)
  :init
  (add-hook 'paredit-mode-hook (lambda () (electric-indent-local-mode 0)))
  :config
  (define-minor-mode paredit-eval-expression-mode
    "Fixes RET in minibuffer with paredit"
    :keymap (make-sparse-keymap)
    (define-key paredit-eval-expression-mode-map
      (kbd "RET")
      #'exit-minibuffer))

  (add-hook 'eval-expression-minibuffer-setup-hook
            #'paredit-eval-expression-mode))

(use-package lispyville
  :diminish
  :ghook 'paredit-mode-hook
  :general
  (general-def normal lispyville-mode-map
    ", R" #'lispyville-drag-backward
    ", r" #'lispyville-drag-forward)
  (general-def (normal visual motion) lispyville-mode-map
    "(" #'lispyville-backward-up-list
    ")" #'lispyville-up-list
    "H" #'lispyville-backward-sexp
    "L" #'lispyville-forward-sexp)
  :config
  (lispyville-set-key-theme
   '(operators
     text-objects
     slurp/barf-cp
     c-w)))

(evil-define-operator hy/cider-eval (beg end type)
  :move-point nil
  (cider-interactive-eval nil
			  nil
			  (list beg end)
			  (cider--nrepl-pr-request-plist)))

(evil-define-operator hy/cider-eval-replace (beg end type)
  :move-point nil
  (let ((form (buffer-substring-no-properties beg end)))
    ;; Only delete the form if the eval succeeds
    (cider-nrepl-sync-request:eval form)
    (delete-region beg end)
    (cider-interactive-eval form
			    (cider-eval-print-handler)
			    nil
			    (cider--nrepl-pr-request-plist))))

(evil-define-operator hy/cider-eval-popup (beg end type)
  :move-point nil
  (cider--pprint-eval-form (buffer-substring-no-properties beg end)))

(use-package clojure-mode
  :general
  (general-def normal clojure-mode-map
    ", j n s" #'clojure-sort-ns
    ", j j" #'cider-jack-in
    ", j c" #'cider-jack-in-cljs
    ", j b" #'cider-jack-in-clj&cljs)
  :init
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :general
  (general-def normal cider-mode-map
    ", e" #'hy/cider-eval
    ", d" #'hy/cider-eval-popup
    ", x" #'hy/cider-eval-replace
    ", f" #'cider-eval-defun-at-point
    ", k" #'cider-load-buffer
    ", t t" #'cider-test-run-test
    ", t a" #'cider-test-rerun-test
    ", t n" #'cider-test-run-ns-tests)
  (general-def normal cider-repl-mode-map
    "g o" #'cider-repl-switch-to-other)
  (general-def normal (cider-mode-map cider-repl-mode-map)
    ", s q" #'sesman-quit
    ", s r" #'sesman-restart)
  (general-def insert cider-repl-mode-map
    "RET" #'cider-repl-return)
  :init
  (setq cider-font-lock-dynamically nil
        cider-repl-display-help-banner nil
        cider-connection-message-fn (lambda () ""))
  ;; Shouldn't be necessary, but it is.
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package company
  :diminish
  :init
  (global-company-mode)
  (company-tng-mode))

(use-package ivy
  :diminish
  :init
  (setq ivy-extra-directories nil)
  :config
  (ivy-mode))

(use-package counsel
  :diminish
  :init
  (counsel-mode)
  :general
  (general-spc "f r" #'counsel-recentf))

(use-package swiper
  :general
  ("C-s" #'swiper))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; Sleuth
(use-package dtrt-indent
  :diminish
  :init
  (dtrt-indent-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(provide 'init)
;;; init.el ends here
