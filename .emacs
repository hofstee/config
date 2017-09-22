(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
			 '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;; Bootstrap `quelpa'
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; Get `quelpa-use-package'
(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"))
(eval-when-compile (require 'quelpa-use-package))
(setq use-package-always-ensure t)

;; Packages Currently Installed
(use-package kaolin-theme
  :pin melpa-stable
  :config
  (load-theme 'kaolin t))
;; (use-package magit
;;   :config
;;   (bind-key* "C-x g" (lambda () (interactive) (magit-status))))
(use-package esup)
(use-package org)
(use-package paredit)
(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (slime-setup '(slime-company)))
(use-package verilog-mode
  :quelpa (verilog-mode :fetcher github :repo "veripool/verilog-mode"))
(use-package lua-mode)
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
(use-package yasnippet
  :config (yas-global-mode 1))
(use-package smart-tabs-mode
  ;; :quelpa (smart-tabs-mode :fetcher github :repo "jcsalomon/smarttabs")
  :config
  ;; (smart-tabs-add-language-support lua lua-mode-hook
  ;;   ((lua-indent-line . lua-indent-level)))
  (smart-tabs-insinuate 'c 'c++))
(use-package smartparens
  :config
  (setq sp-show-pair-delay 0)
  (setq sp-show-pair-from-inside t)
  (show-smartparens-global-mode t))
(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history t)
  (defalias 'redo 'undo-tree-redo)
  (bind-keys*
   ("C-z"   . (lambda () (interactive) (undo-tree-undo 1)))
   ("C-S-z" . (lambda () (interactive) (undo-tree-redo 1)))))
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (use-package company-lua
	:config
	(add-to-list 'company-backends 'company-lua))
  (use-package slime-company
	:config
	(add-to-list 'company-backends 'slime-company)))
;; (use-package irony
;;   :config
;;   (add-hook 'c++-mode-hook  'irony-mode)
;;   (add-hook 'c-mode-hook    'irony-mode))
(use-package flycheck
  :config
  (global-flycheck-mode))
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))
;; (use-package flycheck-irony
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(use-package wakatime-mode
  :config
  (global-wakatime-mode))
(use-package powerline
  :config
  (powerline-default-theme))
;; (use-package telephone-line
;;   :config
;;   (setq telephone-line-lhs
;;       '((accent . (telephone-line-vc-segment
;;                    telephone-line-erc-modified-channels-segment
;;                    telephone-line-process-segment))
;;         (nil    . (telephone-line-minor-mode-segment
;;                    telephone-line-buffer-segment))))
;;   (setq telephone-line-rhs
;; 		'((nil    . (telephone-line-misc-info-segment))
;; 		  (accent . (telephone-line-major-mode-segment))
;; 		  (evil   . (telephone-line-airline-position-segment))))
;;   (setq telephone-line-primary-right-separator   'telephone-line-abs-left
;; 		telephone-line-secondary-right-separator 'telephone-line-abs-hollow-left)
;;   (telephone-line-mode 1))
;; (use-package sml-modeline
;;   :config
;;   (sml-modeline-mode 1))

;; irony-mode
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Personal preferences
(setq frame-resize-pixelwise t)

(set-cursor-color "darkgoldenrod1")
(add-to-list 'default-frame-alist '(cursor-color . "darkgoldenrod1"))
(add-to-list 'default-frame-alist '(cursor-type  . box))
(let ((default-font "NotoMono-10"))
;; (let ((default-font "Roboto Mono for Powerline-11"))
  (progn (add-to-list 'default-frame-alist '(font . default-font))
		 (set-face-attribute 'default nil :font default-font)
		 (set-face-attribute 'default t :font default-font)))

(defun powerline-hud (face1 face2 &optional width)
  "Return an XPM of relative buffer location using FACE1 and FACE2 of optional WIDTH."
  (unless width (setq width 2))
  (let ((color1 (if face1 (face-background face1) "None"))
        (color2 (if face2 (face-background face2) "None"))
        (height (or powerline-height (frame-char-height)))
        pmax
        pmin
        (ws (window-start))
        (we (window-end)))
    (save-restriction
      (widen)
      (setq pmax (point-max))
      (setq pmin (point-min)))
    (pl/percent-xpm height pmax pmin we ws
                    (* (frame-char-width) width) color1 color2)))

(setq tramp-default-method "ssh")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(defun align-comments (beginning end)
  "Align comments within marked region."
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beginning end (concat "\\(\\s-*\\)"
										(regexp-quote comment-start)))))

(defun comment-or-uncomment-lines ()
  "Comments or uncomments the selected line(s)."
  (interactive)
  (let (beg end)
    (if (region-active-p)
		(progn (setq beg (line-beginning-position (+ (- (count-lines 1 (region-beginning)) (count-lines 1 (point))) 1)))
			   (setq end (line-end-position (+ (- (count-lines 1 (region-end)) (count-lines 1 (point))) 1))))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(setq scroll-step 1)                                ;; keyboard scroll one line at a time

(setq fast-but-imprecise-scrolling 't) ;; make scrolling faster

;; Personal global keybindings
(bind-keys*
 ;; Toggle comments for selected lines
 ("C-/"       . (lambda () (interactive) (comment-or-uncomment-lines)))
 ;; C-up/down to scroll the buffer without moving the point
 ("C-<up>"    . (lambda () (interactive) (scroll-down 3)))
 ("C-<down>"  . (lambda () (interactive) (scroll-up   3)))
 ;; M-up/down/left/right to switch window focus
 ("M-<prior>" . (lambda () (interactive) (windmove-up)))
 ("M-<next>"  . (lambda () (interactive) (windmove-down)))
 ("M-<left>"  . (lambda () (interactive) (windmove-left)))
 ("M-<right>" . (lambda () (interactive) (windmove-right))))

;; Save previous location for each file
(if (version< emacs-version "25.1")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))
(setq save-place-file (concat user-emacs-directory "places"))

;; Backup Files Configuration
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t    ; backup of a file the first time it is saved.
      backup-by-copying t    ; don't clobber symlinks
      version-control t      ; version numbers for backup files
      delete-old-versions t  ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6    ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9    ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t    ; auto-save every buffer that visits a file
      auto-save-timeout 20   ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
      )

;; Globally disable tabs, re-enable on a per language basis
(setq-default indent-tabs-mode nil
			  tab-width 4)

;; Don't convert tabs to spaces on backspace.
(setq backward-delete-char-untabify-method nil)


;; Utility functions for easy adding to hooks.
(defun enable-indent-tabs-mode ()
  "Set `indent-tabs-mode' to t in the current buffer."
  (setq indent-tabs-mode t))
(defun disable-indent-tabs-mode ()
  "Set `indent-tabs-mode' to nil in the current buffer."
  (setq indent-tabs-mode nil))

;; Before save hooks
(add-hook 'before-save-hook #'whitespace-cleanup)

;; C Preferences
(setq-default c-default-style "stroustrup"
			  c-basic-offset 4)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(multiple-cursors yaml-mode slime-company company-lua company flycheck-irony flycheck spaceline spaceline-config powerline zenburn-theme yasnippet verilog-mode undo-tree telephone-line sml-modeline smartparens smart-tabs-mode slime quelpa-use-package paredit lua-mode kaolin-theme irony esup)))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
