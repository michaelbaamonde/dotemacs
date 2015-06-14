;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package installation/initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages '(ac-cider
                      ace-window
                      ace-jump
                      cider
		      clojure-mode
                      company
		      darkburn-theme
		      helm
		      helm-git-grep
                      magit
                      paredit
                      savehist
                      saveplace
                      use-package
                      yasnippet))

(when (null package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance and defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Colors
(load-theme 'darkburn t)

(custom-set-faces
 '(region ((t (:background "dim gray")))))

;; Font
(set-face-attribute 'default nil :height 100)
(set-default-font "Inconsolata")

;; Keep a minimal fringe.
(fringe-mode 4)

;; Ignore GUI stuff.
(menu-bar-mode -1)

(when (window-system)
  (set-scroll-bar-mode 'nil)
  (tool-bar-mode -1)
  (mouse-wheel-mode t)
  (tooltip-mode -1))

;; Don't blink.
(blink-cursor-mode -1)

;; Tildes
(global-vi-tilde-fringe-mode t)

;; Show full file path.
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Auto refresh buffers when edits occur outside emacs
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8
(setq locale-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)

(set-keyboard-coding-system 'utf-8)

(set-selection-coding-system 'utf-8)

(prefer-coding-system 'utf-8)

;; Parens
(show-paren-mode 1)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Smooth Scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) .1))) ;; one line at a time

;; Scroll one line when hitting bottom of window
(setq scroll-conservatively 10000)

;; Change Cursor
(setq-default cursor-type 'box)
(blink-cursor-mode -1)

;; Remove alarm (bell) on scroll
(setq ring-bell-function 'ignore)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Word Wrap (t is no wrap, nil is wrap)
(setq-default truncate-lines nil)

;; Sentences do not need double spaces to end.
(set-default 'sentence-end-double-space nil)

;; No startup message.
(setq inhibit-startup-message t)

;; Use text-mode for the scratch buffer.
(setq initial-major-mode 'text-mode)

;; Always indent upon RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; From https://gist.github.com/3402786
(defun toggle-maximize-buffer ()
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ace-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c w") 'ace-window)

;; Ace-jump
(global-set-key (kbd "M-l") 'ace-jump-line-mode)

(global-set-key (kbd "M-c") 'ace-jump-mode)

(setq aw-dispatch-always t)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(defvar aw-dispatch-alist
  '((?x aw-delete-window )
    (?m aw-swap-window)
    (?h aw-split-window-vert)
    (?v aw-split-window-horz)
    (?o toggle-maximize-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(helm-mode 1)

;; Switch <TAB> and C-z
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

;; Make sure this works in the terminal.
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

;; Better Helm defaults.
(setq helm-prevent-escaping-from-minibuffer t
            helm-split-window-in-side-p nil
            helm-bookmark-show-location t
            helm-buffers-fuzzy-matching t
            helm-always-two-windows t
            helm-autoresize-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Full-screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;; Restore windows after exiting magit
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(use-package magit
  :defer 2
  :diminish magit-auto-revert-mode
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (bind-key "q" 'magit-quit-session magit-status-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Paredit
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; Rainbow parens
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook '(lambda() (set-fill-column 80)))
(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             (setq indent-line-function (quote insert-tab))))

(setq require-final-newline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sh-indent-for-then 0)

(setq sh-indent-for-do 0)

(setq sh-indent-after-do '+)

(setq sh-indent-for-case-label '*)

(setq sh-indent-for-case-alt '+)

(setq sh-indent-comment t)

(setq sh-basic-offset 2)

(setq sh-indentation 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")

(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helm
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c a") 'helm-git-grep)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c r") 'helm-show-kill-ring)
(global-set-key (kbd "C-c m") 'helm-man-woman)

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

;; Regex-aware search should be the default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Misc

;; Amazingly annoying to copy a line into the kill ring.
;; Stolen from http://emacswiki.org/emacs/CopyingWholeLines
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(global-set-key (kbd "C-c y") 'copy-line)
