;; package installation/initialization
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages '(cider
		      clojure-mode
		      darkburn-theme
		      evil
		      evil-leader
		      helm
		      helm-git-grep
                      magit
                      paredit))

(when (null package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; appearance
(load-theme 'darkburn t)
(set-face-attribute 'default nil :height 100)
(set-default-font "Inconsolata")
(fringe-mode 4)
(menu-bar-mode -1)
(when (window-system)
  (set-scroll-bar-mode 'nil)
  (mouse-wheel-mode t)
  (tooltip-mode -1))
(tool-bar-mode -1)
(blink-cursor-mode -1)

;; evil
(setq evil-want-C-u-scroll t)
(evil-mode 1)

;; evil-leader
(require 'evil-leader) ;; Weird bug.
(setq global-evil-leader-mode t)

;; helm
(helm-mode 1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(setq helm-prevent-escaping-from-minibuffer t
            helm-split-window-in-side-p nil
            helm-bookmark-show-location t
            helm-buffers-fuzzy-matching t
            helm-always-two-windows t)

(defun helm-evil-leader-setup ()
  (evil-leader/set-leader "<SPC>")
  (eval-after-load "helm"
    (progn
      (evil-leader/set-key
        "f" 'helm-find-files
        "l" 'helm-locate
        "y" 'helm-show-kill-ring
        "t" 'helm-top
        "m" 'helm-man-woman
        "o" 'helm-occur
        ":" 'helm-M-x
        "b" 'helm-mini
        "a" 'helm-git-grep))))

(helm-evil-leader-setup)

;; magit
(setq magit-last-seen-setup-instructions "1.4.0")

(defun magit-evil-leader-setup ()
  (evil-leader/set-leader "<SPC>")
  (eval-after-load "magit"
    (evil-leader/set-key "g" 'magit-status)))

(magit-evil-leader-setup)

;; text editing
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
  '(lambda() (set-fill-column 80)))

(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             (setq indent-line-function (quote insert-tab))))

;; sh-mode
(setq sh-indent-for-then 0)
(setq sh-indent-for-do 0)
(setq sh-indent-after-do '+)
(setq sh-indent-for-case-label '*)
(setq sh-indent-for-case-alt '+)
(setq sh-indent-comment t)
(setq sh-basic-offset 2)
(setq sh-indentation 2)
