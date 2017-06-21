(in-package :stumpwm)

;; load external rc files
(defvar *load-directory*
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname)
                              ".stumpwm.d")))
  "A directory with initially loaded files.")

(defun load-file (filename)
  "Load a file FILENAME (without extension) from `*load-directory*'."
  (let ((file (merge-pathnames (concat filename ".lisp")
                               *load-directory*)))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))

(defun load-files (filenames)
  "Load a list of files (without extensions) from `*load-directory*'."
  (mapcar #'load-file filenames))

(load-files '("gaps"))

;; ;; Visual preferences
;; ;; (load-module "ttf-fonts")
;; ;; (set-font "NotoMono-10")

;; (set-fg-color "white")
;; (set-bg-color "black")
;; (set-border-color "darkgoldenrod1")
;; (set-msg-border-width 1)

;; ;; (set-frame-outline-width 0)
;; (set-win-bg-color "black")
;; (set-focus-color "darkgoldenrod1")
;; (set-unfocus-color "black")

;; ;; (setf *mode-line-border-width* 0)
;; (setf *maxsize-border-width* 1000)
;; (setf *transient-border-width* 1000)
;; (setf *normal-border-width* 1000)
;; (setf *window-border-style* :tight)
;; ;; (set-float-focus-color "")
;; ;; (set-float-unfocus-color "")

;; (setf *message-window-padding* 2)
;; (setf *message-window-gravity* :top-right)
;; (setf *timeout-wait* 1)
;; (setf *input-window-gravity* :top-right)

(defvar *battery-status-command*
  "acpi -b | awk -F '[ ,]' '{printf \"%s%s\", $3, $5}' | sed s/Discharging/\-/ | sed s/Unknown// | sed s/Full// | sed s/Charging/+/")

(setf *screen-mode-line-format*
      (list "[^B%n^b] %W^>"
			'(:eval (run-shell-command *battery-status-command* t))
			" | %d"))

(setf *window-format* "%m%n%s%c")

(setf *mode-line-timeout* 1)

;; Turn on the new mode line.
(toggle-mode-line (current-screen)
				  (current-head))

;; Toggle back and forth between single window
;; (defparameter *layouts* (make-hash-table :test #'eql))
;; (defcommand toggle-full-layout () ()
;; 			(let* ((gnum    (group-number (current-group)))
;; 				   (currlay (gethash gnum *layouts*)))
;; 			  (if currlay
;; 				  (progn
;; 					(restore-group-from-file (current-group) currlay)
;; 					(setf (gethash gnum *layouts*) nil))
;; 				  (progn
;; 					(setf (gethash gnum *layouts*) (dump-group-to-file (current-group)))
;; 					(run-commands "only")))))
;; (define-key *root-map* (kbd "F11") "toggle-full-layout")

(run-shell-command "feh --bg-scale '/home/hofstee/Downloads/wallpaper.png'")

;; Misc preferences
(setf *mouse-focus-policy* :click)

;; Custom keybindings
(define-key *root-map* (kbd "c")   "exec urxvt")
(define-key *root-map* (kbd "C-c") "exec urxvt")
(define-key *root-map* (kbd "C-Up")    "move-focus up")
(define-key *root-map* (kbd "C-Down")  "move-focus down")
(define-key *root-map* (kbd "C-Left")  "move-focus left")
(define-key *root-map* (kbd "C-Right") "move-focus right")
