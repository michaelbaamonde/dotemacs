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
                      avy
                      cider
                      clojure-mode
                      company
                      es-mode
                      haskell-mode
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance and defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package darkburn-theme
  :ensure t
  :init (load-theme 'darkburn t))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'automatic)
  (sml/setup)
  :config
  (setq sml/shorten-directory t
        sml/shorten-modes t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "dim gray"))))
 '(sml/filename ((t (:inherit sml/global :foreground "slate gray" :weight bold)))))

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
(use-package vi-tilde-fringe
  :ensure t
  :init (global-vi-tilde-fringe-mode t))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ace-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From https://gist.github.com/3402786
(defun toggle-maximize-buffer ()
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

(global-set-key (kbd "C-c w") 'ace-window)

;; Ace/Avy
(global-set-key (kbd "M-l") 'avy-goto-line)

(global-set-key (kbd "M-c") 'avy-goto-char)

(setq ace-jump-mode-scope 'window)

(setq aw-dispatch-always t)

(setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))

(defvar aw-dispatch-alist
'((?x aw-delete-window " Ace - Delete Window")
    (?m aw-swap-window " Ace - Swap Window")
    ;; The vert/horz distinction is so counterintuitive
    (?v aw-split-window-horz " Ace - Split Horz Window")
    (?h aw-split-window-vert " Ace - Split Vert Window")
    (?i delete-other-windows " Ace - Maximize Window")
    (?o toggle-maximize-buffer))
"List of actions for `aw-dispatch-default'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :ensure t
  :init (helm-mode 1)
  :config
  (setq helm-prevent-escaping-from-minibuffer t
        helm-split-window-in-side-p nil
        helm-bookmark-show-location t
        helm-buffers-fuzzy-matching t
        helm-always-two-windows t
        helm-autoresize-mode t)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  :bind
  (("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)
   ("C-c a" . helm-git-grep)
   ("C-c o" . helm-occur)
   ("C-c r" . helm-show-kill-ring)
   ("C-c m" . helm-man-woman)))

(use-package helm-swoop
  :ensure t
  :bind (("C-c ;" . helm-swoop)
         ("C-c M-;" . helm-multi-swoop)))

(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds)
  :init (fset 'describe-bindings 'helm-descbinds))

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
  (bind-key "q" 'magit-quit-session magit-status-mode-map)
  (setq magit-auto-revert-mode t)
  :bind
  ("C-c g" . magit-status))

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

(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook
          'set-auto-complete-as-completion-at-point-function)
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
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Regex-aware search should be the default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Misc editing

;; Amazingly annoying to copy a line into the kill ring.
;; Stolen from http://emacswiki.org/emacs/CopyingWholeLines
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(global-set-key (kbd "C-c y") 'copy-line)

;; Stolen from http://whattheemacsd.com/editing-defuns.el-01.html
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<M-return>") 'open-line-above)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Yes, I really do want to quit.
(require 'cl)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
           (flet ((process-list ())) ad-do-it))

;; Uniquely name buffers in a sane manner
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq transient-mark-mode t)

;; Paredit
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)

;; Org
(setq org-src-fontify-natively t
      org-refile-targets '((nil . (:maxlevel . 2)))
      org-default-notes-file "~/notes/work/notes.org"
      org-return-follows-link t
      org-babel-clojure-backend 'cider
      org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (elasticsearch . t)
   (haskell . t)
   (sh . t)
   (ruby . t)
   (python . t)))

; add a default shebang header argument shell scripts
(add-to-list 'org-babel-default-header-args:sh
             '(:shebang . "#!/usr/bin/env bash"))

(global-set-key
 (kbd "C-c n")
 (lambda ()
   (interactive)
   (find-file "~/notes/work/notes.org")))

(global-set-key (kbd "C-c c") 'org-capture)

;; Misc
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-c s") 'shell)

(global-set-key
 (kbd "C-c e")
 (lambda ()
   (interactive)
   (find-file "~/.emacs")))

;;; Stolen from http://stackoverflow.com/questions/95631/open-a-file-with-su-sudo-inside-emacs
(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; Elasticsearch
(setq es-warn-on-delete-query nil)

(global-set-key
 (kbd "C-c l")
 (lambda ()
   (interactive)
   (find-file "~/es-command.org")))

;; Use Chrome for links
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
