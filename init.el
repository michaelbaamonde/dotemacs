;; package installation/initialization
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages '(ace-jump
                      ace-window
                      ac-cider
                      cider
		      clojure-mode
                      company
		      darkburn-theme
		      evil
		      evil-leader
		      helm
		      helm-git-grep
                      magit
                      paredit
                      use-package))

(when (null package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'use-package)

;; Appearance

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
  (mouse-wheel-mode t)
  (tooltip-mode -1))

(tool-bar-mode -1)

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

;; Always indent upon RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Evil
(setq evil-want-C-u-scroll t)

;; Turn on evil-mode by default.
(evil-mode 1)

;; Vim's default insert state is useless.
(defalias 'evil-insert-state 'evil-emacs-state)

;; Change the cursor color based on state.
(setq evil-emacs-state-cursor '(box "cyan")
      evil-normal-state-cursor '(box "white"))

;; ESC in emacs state should get us back to normal mode.
(global-set-key (kbd "<escape>") 'evil-normal-state)

;; C-c C-c is a nice alternative to ESC, as well.
(global-set-key (kbd "C-c C-c") 'evil-normal-state)

;; Evil Leader
(require 'evil-leader) ;; Weird bug.
(setq global-evil-leader-mode t)

;; From https://gist.github.com/3402786
(defun toggle-maximize-buffer ()
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

;; Possibly the only ergonomic setup for laptop keyboards.
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "wc"  'delete-window
  "wH"  'evil-window-move-far-left
  "wh"  'evil-window-left
  "wJ"  'evil-window-move-very-bottom
  "wj"  'evil-window-down
  "wK"  'evil-window-move-very-top
  "wk"  'evil-window-up
  "wL"  'evil-window-move-far-right
  "wl"  'evil-window-right
  "wm"  'toggle-maximize-buffer
  "ws"  'split-window-below
  "wv"  'split-window-right
  "k"   'evil-delete-buffer
  "d"   'dired
  "p"   'previous-buffer
  "!"   'shell-command
  "j"   'ace-jump-line-mode
  "f"   'helm-find-files
  "l"   'helm-locate
  "y"   'helm-show-kill-ring
  "t"   'helm-top
  "m"   'helm-man-woman
  "o"   'helm-occur
  ":"   'helm-M-x
  "b"   'helm-mini
  "a"   'helm-git-grep
  "g"   'magit-status)

;; Ace-window
(global-set-key (kbd "M-p") 'ace-window)

(setq aw-dispatch-always t)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(defvar aw-dispatch-alist
'((?x aw-delete-window " Ace - Delete Window")
    (?m aw-swap-window " Ace - Swap Window")
    (?n aw-flip-window)
    (?h aw-split-window-vert " Ace - Split Vert Window")
    (?v aw-split-window-horz " Ace - Split Horz Window")
    (?i delete-other-windows " Ace - Maximize Window")
    (?o toggle-maximize-buffer))
"List of actions for `aw-dispatch-default'.")

;; Helm
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

;; Magit

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

;; Clojure

;; Paredit
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Rainbow parens
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

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

;; Text editing

;; Let's clean up after ourselves.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook '(lambda() (set-fill-column 80)))

;; Tabs FTL.
(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             (setq indent-line-function (quote insert-tab))))

(setq require-final-newline t)

;; Shell mode
(setq sh-indent-for-then 0)
(setq sh-indent-for-do 0)
(setq sh-indent-after-do '+)
(setq sh-indent-for-case-label '*)
(setq sh-indent-for-case-alt '+)
(setq sh-indent-comment t)
(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; Company
(add-hook 'after-init-hook 'global-company-mode)
