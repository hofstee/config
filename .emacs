;; Start up el-get with emacs
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; Packages Currently Installed
(setq my-packages
      '(paredit slime verilog-mode whitespace))
(el-get-bundle irony-mode
  (progn (add-hook 'c++-mode-hook 'irony-mode)
		 (add-hook 'c-mode-hook 'irony-mode)
		 (add-hook 'objc-mode-hook 'irony-mode)))
(el-get-bundle yasnippet
  (yas-global-mode 1))
(el-get-bundle smarttabs
  (smart-tabs-insinuate 'c))
(el-get-bundle undo-tree
  (progn (global-undo-tree-mode 1)
		 (global-set-key (kbd "C-z") 'undo)
		 (defalias 'redo 'undo-tree-redo)
		 (global-set-key (kbd "C-S-z") 'redo)))
										;(el-get-bundle zenburn-theme
										;  (load-theme 'zenburn t))
(el-get 'sync my-packages)

;; irony-mode
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Personal preferences
(add-to-list 'default-frame-alist '(cursor-color . "darkgoldenrod1"))
(add-to-list 'default-frame-alist '(cursor-type  . box))

(show-paren-mode 1)
(tool-bar-mode -1)

(defun comment-or-uncomment-lines ()
  "Comments or uncomments the selected line(s)"
  (interactive)
  (let (beg end)
	(if (region-active-p)
		(progn (setq beg (line-beginning-position (+ (- (count-lines 1 (region-beginning)) (count-lines 1 (point))) 1)))
			   (setq end (line-end-position (+ (- (count-lines 1 (region-end)) (count-lines 1 (point))) 1))))		
	  (setq beg (line-beginning-position) end (line-end-position)))
	(comment-or-uncomment-region beg end)))

(defun align-comments (beginning end)
  "Align comments within marked region."
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beginning end (concat "\\(\\s-*\\)"
                                        (regexp-quote comment-start)))))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(setq scroll-step 1)                                ;; keyboard scroll one line at a time

;; Create a minor mode with personal keymappings
(defvar my-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

;; Toggle comments for selected lines
(define-key my-mode-map (kbd "C-/") 'comment-or-uncomment-lines)
;; C-up/down to scroll the buffer without moving the point
(define-key my-mode-map (kbd "C-<prior>") (lambda () (interactive (scroll-down 2))))
(define-key my-mode-map (kbd "C-<next>")  (lambda () (interactive (scroll-up   2))))
;; M-up/down/left/right to switch window focus
(define-key my-mode-map (kbd "M-<prior>") (lambda () (interactive) (windmove-up)))
(define-key my-mode-map (kbd "M-<next>")  (lambda () (interactive) (windmove-down)))
(define-key my-mode-map (kbd "M-<left>")  (lambda () (interactive) (windmove-left)))
(define-key my-mode-map (kbd "M-<right>") (lambda () (interactive) (windmove-right)))

(define-minor-mode my-mode
  "A minor mode to override major mode key rebinds."
  :global     t
  :init-value t
  :keymap     my-mode-map)

(my-mode 1)

;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-my-mode ()
  "Turn off my-mode."
  (my-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-mode)

;; Save previous location for each file
(if (version< emacs-version "25.1")
	(progn
	  (require 'saveplace)
	  (setq-default save-place t))
  ((save-place-mode 1)))
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

;; C Preferences
(setq-default c-default-style "stroustrup"
			  c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)

;; Custom preferences
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
	("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" default)))
 '(fci-rule-color "#383838")
 '(inhibit-startup-screen t)
 '(nrepl-message-colors
   (quote
	("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
	((20 . "#BC8383")
	 (40 . "#CC9393")
	 (60 . "#DFAF8F")
	 (80 . "#D0BF8F")
	 (100 . "#E0CF9F")
	 (120 . "#F0DFAF")
	 (140 . "#5F7F5F")
	 (160 . "#7F9F7F")
	 (180 . "#8FB28F")
	 (200 . "#9FC59F")
	 (220 . "#AFD8AF")
	 (240 . "#BFEBBF")
	 (260 . "#93E0E3")
	 (280 . "#6CA0A3")
	 (300 . "#7CB8BB")
	 (320 . "#8CD0D3")
	 (340 . "#94BFF3")
	 (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
