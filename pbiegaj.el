;;;; line numbers on the left in a gui
(global-linum-mode 1)

;; don't iconify on C-z when running in X
;; or exit emacs (!) when running in Emacs.app
(when window-system (global-unset-key "\C-z"))

(setq kill-whole-line t)
(setq confirm-kill-emacs 'yes-or-no-p)

;; 4 Macs
(setq mac-command-modifier 'meta)

(add-to-list 'load-path "~/.emacs.d/elpa/solarized-theme-0.2")
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/solarized-theme-0.2")
(load-theme 'solarized-dark t)

(push "/usr/local/bin" exec-path)
;; no more backup files emacs, I got it
(setq make-backup-files nil)
;; set font type and size
(set-default-font "Monaco-9")
;; Take all the windows in the current frame and shift them over one.
;; ;;
;; ;; With 2 windows, effectively switches their positions.
;; ;;
;; ;; With 1 window, this is a no-op.
(defun rotate-windows ()
  (interactive)
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (mapcar* 'set-window-buffer
             (window-list)
             (append (cdr buffers) (list (car buffers))))))

;; load flymake-ruby mode for files flagged for ruby-mode
(custom-set-faces
 '(flymake-errline ((((class color)) (:background "#ffffd7"))))
 '(flymake-warnline ((((class color)) (:background "#0a2832")))))

(eval-after-load 'ruby-mode
  '(progn
     (add-hook 'ruby-mode-hook 'flymake-ruby-load)))


;;;; spiffy-mode provides a few utilities
;; (add-to-list 'load-path "~/.emacs.d/vendor/spiffy")
;; (setq spiffy-enable-minor-mode t)
;; (require 'spiffy)

;;;; spiffy-textmate-mode has some good stuff, but I don't want the
;;;; full minor mode since it stomps all over a bunch of default keybindings
;; (require 'spiffy-textmate-mode)
;; (global-set-key [(f5)] 'spiffy-tm-grep-project)
