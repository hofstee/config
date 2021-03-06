(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
;; (package-refresh-contents 1)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; ;; Bootstrap `use-package'
;; (eval-when-compile
;;   ;; Following line is not needed if use-package.el is in ~/.emacs.d
;;   (add-to-list 'load-path "<path where use-package is installed>")
;;   (require 'use-package))

;; Bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)

;; Packages Currently Installed
(use-package kaolin-themes :ensure t
  :config
  (load-theme 'kaolin-dark t)
  (defun dark ()
    (interactive)
    (load-theme 'kaolin-dark t))
  (defun light ()
    (interactive)
    (load-theme 'kaolin-light t)))

;; Highlight the portion of code currently being worked on
(use-package focus :ensure t
  :config
  (add-to-list 'focus-mode-to-thing '(python-mode . paragraph)))

;; Dim other currently open windows
(use-package dimmer :ensure t
  :config
  (dimmer-mode t))

(use-package magit :ensure t
  :config
  (use-package magit-todos :ensure t
    :config
    (magit-todos-mode))
  (bind-key* "C-x g" (lambda () (interactive) (magit-status))))

(use-package git-gutter :ensure t
  :init (global-git-gutter-mode 1))

;; Useful package for editing files that have their own ideas about indentation
(use-package dtrt-indent :ensure t
  :config
  (dtrt-indent-mode))

(use-package ws-butler :ensure t
  :config
  (ws-butler-global-mode))

(use-package esup :ensure t)

(use-package keyfreq :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package elmacro :ensure t
  :init (elmacro-mode))

;; Installing org with `straight.el'
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(use-package org
  :straight t
  :config
  (use-package org-tempo
    :config
    (add-to-list 'org-tempo-keywords-alist '("n" . "name")))
  (use-package org-wiki
    :straight (org-wiki :type git :host github :repo "caiorss/org-wiki")
    :config
    (setq org-wiki-location-list '("~/private/wiki"))
    (setq org-wiki-location (car org-wiki-location-list))
    (setq org-wiki-template "")
    (advice-add 'org-wiki-header :override
                (lambda ()
                  "Insert wiki-template with yasnippet when creating new entry."
                  (set-mark 0)
                  (yas-expand-snippet (yas-lookup-snippet "wiki-template"))
                  (hack-local-variables))
                '((name . "insert-wiki-header"))))
  (setq org-adapt-indentation nil
        org-startup-with-inline-images t
        org-support-shift-select t
        org-catch-invisible-edits 'smart
        org-cycle-separator-lines 0
        org-log-done 'time)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  ;; (add-hook 'org-mode-hook
  ;;           (lambda () (setq org-image-actual-width (window-body-width nil t))))
  ;; (add-hook 'window-size-change-functions
  ;;           (lambda (frame) (setq org-image-actual-width (window-body-width nil t))))
  ;; (add-hook 'window-size-change-functions
  ;;           (lambda (frame) (setq org-image-actual-width (list (window-body-width nil t)))))
  (require 'ob-lua)
  (defun org-babel-execute:wavedrom (body params)
    "Executes wavedrom-cli with org-babel."
    (let ((tempfile (make-temp-file "wavedrom")))
      (with-temp-buffer
        (insert body)
        (write-region nil nil tempfile))
      (print (concat "wavedrom -i" tempfile))
      (org-babel-eval (concat "wavedrom -i " tempfile) "")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lua . t))))

;; (use-package slime
;;   :config
;;   (setq inferior-lisp-program "sbcl")
;;   (load (expand-file-name "~/tools/quicklisp/slime-helper.el"))
;;   (slime-setup '(slime-company)))

(use-package verilog-mode
  :straight (verilog-mode :type git :host github :repo "veripool/verilog-mode")
  :config
  (setq verilog-indent-level             3
        verilog-indent-level-module      3
        verilog-indent-level-declaration 3
        verilog-indent-level-behavioral  3
        verilog-indent-level-directive   1
        verilog-case-indent              2
        verilog-auto-newline             nil
        verilog-auto-indent-on-newline   t
        verilog-tab-always-indent        t
        verilog-auto-endcomments         nil
;;         verilog-minimum-comment-distance 40
;;         verilog-indent-begin-after-if    t
        verilog-auto-lineup              nil
;;         verilog-linter                   "my_lint_shell_command"
        ))

(use-package lua-mode :ensure t)

(use-package go-mode :ensure t)

(use-package rust-mode :ensure t
  :config
  (bind-keys*
   ("C-c C-c" . rust-run)))

(use-package cmake-mode :ensure t)

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     (setq treemacs-follow-after-init          t
;;           treemacs-width                      35
;;           treemacs-indentation                2
;;           treemacs-git-integration            t
;;           treemacs-collapse-dirs              3
;;           treemacs-silent-refresh             nil
;;           treemacs-change-root-without-asking nil
;;           treemacs-sorting                    'alphabetic-desc
;;           treemacs-show-hidden-files          t
;;           treemacs-never-persist              nil
;;           treemacs-is-never-other-window      nil
;;           treemacs-goto-tag-strategy          'refetch-index)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t))
;;   :bind
;;   (:map global-map
;;         ([f8]         . treemacs-toggle)
;;         ("M-0"        . treemacs-select-window)
;;         ("C-c 1"      . treemacs-delete-other-windows)
;;         ("M-m ft"     . treemacs-toggle)
;;         ("M-m fT"     . treemacs)
;;         ("M-m fB"     . treemacs-bookmark)
;;         ("M-m f C-t"  . treemacs-find-file)
;;         ("M-m f M-t"  . treemacs-find-tag)))
;; (use-package treemacs-projectile
;;   :defer t
;;   :ensure t
;;   :config
;;   (setq treemacs-header-function #'treemacs-projectile-create-header)
;;   :bind (:map global-map
;;               ("M-m fP" . treemacs-projectile)
;;               ("M-m fp" . treemacs-projectile-toggle)))

(use-package dashboard :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package yaml-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.cheby\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.shapo\\'" . yaml-mode)))

(use-package bazel-mode :ensure t)

(use-package yasnippet :ensure t
  :config (yas-global-mode 1))

;; (use-package smartparens :ensure t
;;   :config
;;   (setq sp-show-pair-delay 0)
;;   (setq sp-show-pair-from-inside t)
;;   (show-smartparens-global-mode t))

;; (use-package undo-tree :ensure t
;;   :config
;;   (global-undo-tree-mode 1)
;;   (setq undo-tree-auto-save-history t)
;;   (defalias 'redo 'undo-tree-redo)
;;   (bind-keys*
;;    ("C-z"   . undo)
;;    ("C-S-z" . redo)))

(use-package undo-fu :ensure t
  :config
  (bind-keys*
   ("C-z"   . undo-fu-only-undo)
   ("C-S-z" . undo-fu-only-redo)))

(use-package eglot :ensure t)

;; (use-package company :ensure t
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   ;; (use-package company-lua :ensure t
;;   ;;   :config
;;   ;;   (add-to-list 'company-backends 'company-lua))
;;   ;; (use-package slime-company :ensure t
;;   ;;   :config
;;   ;;   (add-to-list 'company-backends 'slime-company))
;;   (use-package company-tabnine :ensure t
;;     :config
;;     ;; Trigger completion immediately.
;;     (setq company-idle-delay 0)

;;     ;; Number the candidates (use M-1, M-2 etc to select completions).
;;     (setq company-show-numbers t)

;;     ;; Use the tab-and-go frontend.
;;     ;; Allows TAB to select and complete at the same time.
;;     (company-tng-configure-default)
;;     (setq company-frontends
;;           '(company-tng-frontend
;;             company-pseudo-tooltip-frontend
;;             company-echo-metadata-frontend))))

;; (use-package irony
;;   :config
;;   (add-hook 'c++-mode-hook  'irony-mode)
;;   (add-hook 'c-mode-hook    'irony-mode))

(use-package flycheck :ensure t
  :config
  (setq flycheck-global-modes (not 'org-mode))
  (global-flycheck-mode))

(use-package multiple-cursors :ensure t
  :config
  (setq mc/edit-lines-empty-lines 'ignore) ; ignore short lines
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package phi-search :ensure t
  :config
  (global-set-key (kbd "C-s") 'phi-search)
  (global-set-key (kbd "C-r") 'phi-search-backward))

;; (use-package flycheck-irony
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package persistent-scratch :ensure t
  :config
  (persistent-scratch-setup-default))

(use-package deft
  :straight t
  :bind ("<f8>" . deft)
  :commands (deft)
  :config
  (setq deft-recursive t
        deft-directory "~/private/deft"
        deft-extensions '("md" "org" "tex" "txt")
        deft-default-extension "org"
        deft-auto-save-interval 0.0
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "_")
                                 (case-fn . downcase)))
  (add-hook 'deft-open-file-hook (lambda () (kill-buffer "*Deft*")))
  (add-hook 'deft-open-file-hook
            (lambda ()
              "Rename buffer to title of note upon opening."
              (let ((title (deft-file-title (buffer-file-name))))
                (if title (rename-buffer title t)))))
  (advice-add 'deft-new-file :after
              (lambda ()
                "Insert deft-template with yasnippet when creating new notes."
                (whitespace-cleanup)
                (yas-expand-snippet (yas-lookup-snippet "deft-template"))
                (hack-local-variables)
                (save-buffer))
              '((name . "insert-yasnippet-template")))
  (define-key deft-mode-map (kbd "C-<backspace>") 'deft-filter-decrement-word))

;; (use-package pdf-tools :ensure t
;;   :config
;;   (pdf-loader-install))

;; (use-package wakatime-mode :ensure t
;;   :config
;;   (global-wakatime-mode))

;; (use-package powerline
;;   :config
;;   (powerline-default-theme))

;; (use-package telephone-line
;;   :config
;;   (setq telephone-line-lhs
;;       '((accent . (telephone-line-vc-segment
;;                    telephone-line-erc-modified-channels-segment
;;                    telephone-line-process-segment))
;;         (nil    . (telephone-line-minor-mode-segment
;;                    telephone-line-buffer-segment))))
;;   (setq telephone-line-rhs
;;      '((nil    . (telephone-line-misc-info-segment))
;;        (accent . (telephone-line-major-mode-segment))
;;        (evil   . (telephone-line-airline-position-segment))))
;;   (setq telephone-line-primary-right-separator   'telephone-line-abs-left
;;      telephone-line-secondary-right-separator 'telephone-line-abs-hollow-left)
;;   (telephone-line-mode 1))

;; (use-package sml-modeline
;;   :config
;;   (sml-modeline-mode 1))

(use-package ghub :ensure t)

(use-package doom-modeline :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-icon nil
        doom-modeline-github t))

;; (use-package dumb-jump :ensure t
;;   :bind
;;   (("M-g o" . dumb-jump-go-other-window)
;;    ("M-g j" . dumb-jump-go)
;;    ("M-g i" . dumb-jump-go-prompt)
;;    ("M-g x" . dumb-jump-go-prefer-external)
;;    ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;   :config
;;   (setq dumb-jump-selector 'ivy))
;;   ;; (setq dumb-jump-selector 'helm))

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
(global-visual-line-mode 1)

;; Disable all extra UI bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Replace highlighted region with what I type
(delete-selection-mode 1)

(setq column-number-mode t)

(set-cursor-color "darkgoldenrod1")
(add-to-list 'default-frame-alist '(cursor-color . "darkgoldenrod1"))
(add-to-list 'default-frame-alist '(cursor-type  . box))
;; (let ((default-font "NotoMono-13"))
;; (let ((default-font "Roboto Mono for Powerline-11"))
;; (let ((default-font "DejaVu Sans Mono-14"))
;;   (progn (add-to-list 'default-frame-alist '(font . default-font))
;;          (set-face-attribute 'default nil :font default-font)
;;          (set-face-attribute 'default t :font default-font)))
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-14"))

;; Make Emacs handle long lines better
;; (global-so-long-mode 1)

;; (use-package vlf :ensure t
;;   :config
;;   (require 'vlf-setup)
;;   (custom-set-variables
;;    '(vlf-application 'dont-ask)))

(setq-default bidi-display-reordering nil)

;; (use-package fzf :ensure t)

(use-package find-file-in-project :ensure t
  :config
  (setq ffip-use-rust-fd t))

(use-package anzu :ensure t
  :config
  (global-anzu-mode 1))

(use-package helm :ensure t
  :config
  (require 'helm)
  (require 'helm-config)
  (setq helm-move-to-line-cycle-in-source nil)

  ;; ; Also kinda slow...
  ;; (use-package helm-swoop :ensure t
  ;;   :config
  ;;   (global-set-key (kbd "C-s") 'helm-swoop))

  ;; ; This is much slower than swiper using ivy for some reason
  ;; (use-package swiper-helm :ensure t
  ;;   :config
  ;;   (global-set-key (kbd "C-s") 'swiper-helm))

  ;; (use-package swiper :ensure t
  ;;   :config
  ;;   (require 'counsel)
  ;;   (require 'ivy)
  ;;   (global-set-key (kbd "C-s") 'counsel-grep-or-swiper))

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (when (executable-find "rg")
    (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s"
          helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'")))

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t   ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     nil ; allow moving between sources in helm results
        helm-ff-search-library-in-sexp        t   ; search for library in `require' and `declare-function' sexp .
        helm-scroll-amount                    8   ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)

  (defun spacemacs//helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))


  (add-hook 'helm-minibuffer-set-up-hook
            'spacemacs//helm-hide-minibuffer-maybe)

  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)

  (helm-mode 1))

(use-package helm-rg :ensure t)

(use-package helm-org-rifle :ensure t)

(use-package lsp-mode :ensure t
  :hook ((python-mode . lsp)
         (rust-mode   . lsp)))

(use-package tuareg :ensure t)

(use-package string-inflection :ensure t
  :bind
  (("C-c C-u" . string-inflection-all-cycle)))

;; (defun powerline-hud (face1 face2 &optional width)
;;   "Return an XPM of relative buffer location using FACE1 and FACE2 of optional WIDTH."
;;   (unless width (setq width 2))
;;   (let ((color1 (if face1 (face-background face1) "None"))
;;         (color2 (if face2 (face-background face2) "None"))
;;         (height (or powerline-height (frame-char-height)))
;;         pmax
;;         pmin
;;         (ws (window-start))
;;         (we (window-end)))
;;     (save-restriction
;;       (widen)
;;       (setq pmax (point-max))
;;       (setq pmin (point-min)))
;;     (pl/percent-xpm height pmax pmin we ws
;;                     (* (frame-char-width) width) color1 color2)))

;; Configure tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(add-to-list 'tramp-methods
             '("yadm"
               (tramp-login-program "yadm")
               (tramp-login-args (("enter")))
               (tramp-login-env
                (("SHELL")
                 ("/bin/sh")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args ("-c"))))

(use-package docker-tramp :ensure t)
(use-package dockerfile-mode :ensure t)

;; Save buffers on exit
(desktop-save-mode 1)
;; ;; Force loading desktop file if in use by another process
;; (setq desktop-load-locked-desktop t)
;;  (call-interactively 'desktop-read t (vector "~/.emacs.d/desktops/" t))

;; Custom functions
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

(defun nuke-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(setq scroll-conservatively 101)                    ;; don't center point when scrolling
(setq scroll-margin 5)                              ;; small buffer of lines when moving point

(setq jit-lock-defer-time nil)         ;; don't defer fontification
(setq fast-but-imprecise-scrolling 't) ;; make scrolling faster

;; delete without adding to kill-ring, from ErgoEmacs
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

;; Personal global keybindings
(bind-keys*
 ;; Toggle comments for selected lines
 ("C-/"           . (lambda () (interactive) (comment-or-uncomment-lines)))
 ;; Close all open buffers
 ("C-x K"         . (lambda () (interactive) (nuke-all-buffers)))
 ;; Open `*scratch*' on C-x b
 ("C-x b"         . (lambda () (interactive) (switch-to-buffer (get-buffer-create "*scratch*"))))
 ("C-x C-b"       . (lambda () (interactive) (ibuffer)))
 ;; C-up/down to scroll the buffer without moving the point
 ("C-<up>"        . (lambda () (interactive) (scroll-down 3)))
 ("C-<down>"      . (lambda () (interactive) (scroll-up   3)))
 ;; M-up/down/left/right to switch window focus
 ("M-<up>"        . (lambda () (interactive) (windmove-up)))
 ("<prior>"       . (lambda () (interactive) (windmove-up))) ;; ChromeOS-specific
 ("M-<udown>"     . (lambda () (interactive) (windmove-down)))
 ("<next>"        . (lambda () (interactive) (windmove-down))) ;; ChromeOS-specific
 ("M-<left>"      . (lambda () (interactive) (windmove-left)))
 ("M-<right>"     . (lambda () (interactive) (windmove-right)))
 ;; Better case-conversion commands
 ("M-l"           . downcase-dwim)
 ("M-u"           . upcase-dwim)
 ("M-c"           . capitalize-dwim)
 ;; Don't have C-backspace put things in `kill-ring'
 ("C-<backspace>" . my-backward-delete-word)
 ;; hi-lock shortcuts
 ("M-s M-s"       . (lambda () (interactive) (hi-lock-face-symbol-at-point)))
 ("M-s M-u"       . (lambda () (interactive) (hi-lock-unface-buffer t))))

;; Hide certain buffers while cycling through windows
(set-frame-parameter (selected-frame) 'buffer-predicate
                     (lambda (buf) (and
                                    ;; Buffers starting with '*'
                                    (not (string-match-p "^*" (buffer-name buf)))
                                    ;; magit buffers
                                    (not (string-match-p "^magit.*:" (buffer-name buf))))))


;; Find a browser for opening URLs
;; (setq browse-url-generic-program
;;       (executable-find (getenv "BROWSER"))
;;       browse-url-browser-function 'browse-url-generic)

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

;; Tab settings
(setq-default indent-tabs-mode nil        ; globally disable tabs, re-enable on a per language basis
              tab-width 4
              tab-always-indent 'complete ; indent first then complete if indentation correct
              )

;; Don't convert tabs to spaces on backspace.
(setq backward-delete-char-untabify-method nil)

;; Utility functions for easy adding to hooks.
(defun enable-indent-tabs-mode ()
  "Set `indent-tabs-mode' to t in the current buffer."
  (setq indent-tabs-mode t))
(defun disable-indent-tabs-mode ()
  "Set `indent-tabs-mode' to nil in the current buffer."
  (setq indent-tabs-mode nil))

;; Allow remote dir-locals
(setq enable-remote-dir-locals t)

;; C Preferences
(setq-default c-default-style "stroustrup"
              c-basic-offset 4)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; The very last thing done is modify the behavior of the *scratch* buffer. This
;; way, even if Emacs is running in daemon mode, if the default contents appear
;; instead, I know something went wrong at startup.
(setq initial-scratch-message nil)
