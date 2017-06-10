;;;; set srgb
(setq ns-use-srgb-colorspace t)

;;;; new fullscreen emulating old fullscreen in Emacs 24.3
(setq ns-use-native-fullscreen nil)

;; fullscreen on Meta-Shift-f
(global-set-key "\M-F" 'toggle-frame-fullscreen)

;;;; set column width to 80 unless someone sets higher in a mode
(setq-default fill-column 120)

;;;; set line hl-line-mode to true
(hl-line-mode t)

;;;; line numbers on the left in a gui
(global-linum-mode t)

;; set default directory to be starting from the projects root
(setq default-directory "~/src")

;; add brew binaries to the exec-path
(push "/usr/local/bin" exec-path)

;; no more backup files emacs, I got it
(setq make-backup-files nil)

;; set font type and size
(set-default-font "Monaco-9")

;; auto update of modified files from the filesystem, mostly for
;; sharing two different editors on the same machine
(global-auto-revert-mode t)

;; don't iconify on C-z when running in X
;; or exit emacs (!) when running in Emacs.app
(when window-system (global-unset-key "\C-z"))

;; C-k will kill the whole line
(setq kill-whole-line t)

;; In case I fat finger exit, prompt me first
(setq confirm-kill-emacs 'yes-or-no-p)

;; set cmd key to meta
(setq mac-command-modifier 'meta)

;; transparency stuffs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; just a little transparency plz
(transparency 98)

(defun rvm-two-step ()
  (rvm-activate-corresponding-ruby)
  (flymake-ruby-load))

;; window switch shortcuts
(global-set-key "\M-N" (lambda ()
                          (interactive)
                          (next-multiframe-window)
                          (if (eql major-mode 'ruby-mode) (rvm-two-step))))

(global-set-key "\M-P" (lambda ()
                          (interactive)
                          (previous-multiframe-window)
                          (if (eql major-mode 'ruby-mode) (rvm-two-step))))


;; repository should be restored when loading `bm'
(setq bm-restore-repository-on-load t)
(eval-after-load 'bm
  '(progn
     ;; key binding
     (global-set-key (kbd "\C-x l") 'bm-show-all)
     (global-set-key (kbd "<M-f2>") 'bm-toggle)
     (global-set-key (kbd "<f2>") 'bm-next)
     (global-set-key (kbd "<S-f2>") 'bm-previous)

     ;; cleanup whitespace in a file
     (global-set-key (kbd "\C-x w c") 'whitespace-cleanup)

     ;; disable visual bell
     (setq visible-bell nil)

     ;; look across all buffers when cycling through bookmarks
     (setq bm-cycle-all-buffers t)

     ;; highligh mode for bookmarks
     (setq bm-highlight-style 'bm-highlight-line-and-fringe)

     ;; buffer should be recentered around the bookmark
     (setq bm-recenter t)

     ;; make bookmarks persistent as default
     (setq-default bm-buffer-persistence t)

     ;; loading the repository from file when on start up
     (add-hook 'after-init-hook 'bm-repository-load)

     ;; restoring bookmarks when on file find
     (add-hook 'find-file-hooks 'bm-buffer-restore)

     ;; saving bookmark data on killing a buffer
     (add-hook 'kill-buffer-hook 'bm-buffer-save)

     ;; saving the repository to file when on exit
     ;; `kill-buffer-hook' is not called when emacs is killed, so we
     ;; must save all bookmarks first
     (add-hook 'kill-emacs-hook (lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))

     ;; update bookmark repository when saving the file
     (add-hook 'after-save-hook 'bm-buffer-save)))

;; add js3-mode to load path
(add-to-list 'load-path "~/.emacs.d/non-elpa-libs/js3-mode")
(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

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

(custom-set-faces
 '(flymake-errline ((((class color)) (:background "#ffffd7"))))
 '(flymake-warnline ((((class color)) (:background "#0a2832")))))

;; add rabl files to auto set ruby major mode
(add-to-list 'auto-mode-alist '("\\.rabl$" . ruby-mode))

;; load flymake-ruby mode for files flagged for ruby-mode
(eval-after-load 'ruby-mode
  '(progn
     (add-hook 'ruby-mode-hook (lambda () (rvm-activate-corresponding-ruby) (flymake-ruby-load)))
     (define-key ruby-mode-map [(meta r)] 'spiffy-ruby-run-spec-file)
     (define-key ruby-mode-map [(meta R)] 'spiffy-ruby-run-spec-under-point)
     (define-key ruby-mode-map [(control ?\;) ?r ?t] 'spiffy-ruby-rerun-last-test)
     ))


;;;; color spec output
(defun font-lock-proof (string start)
  (cond
   ((>= start (length string)) "")
   (t
    (let* ((end (next-property-change start string (length string)))
           (s (substring string start end)))
      (set-text-properties 0
                           (length s)
                           (set-face-attribute 'font-lock-face 'face)
                           s)
      (concat s (font-lock-proof string end))))))

(global-set-key [(f5)] 'projectile-grep)
(global-set-key [(f6)] 'next-error)
(global-set-key [(shift f6)] 'previous-error)
(global-set-key "\C-x\C-b" 'buffer-menu)

(defun spiffy-local-file-name ()
  (if (and (boundp 'tramp-file-name-regexp)
           (eq (string-match tramp-file-name-regexp (buffer-file-name)) 0))
      (tramp-file-name-localname (tramp-dissect-file-name (buffer-file-name)))
    (buffer-file-name)))

(defun spiffy-cwd ()
  ; pwd returns "Directory /where/you/are/"; this gets rid of the baloney
  (substring (pwd) 10))

(defmacro spiffy-run-in-directory (dir &rest body)
  "Execute code in a particular current working directory"
  (let ((retval-var (make-symbol "retval"))
        (original-dir-var (make-symbol "original-dir")))
    `(let ((,original-dir-var (spiffy-cwd)))
       (unless (null ,dir) (cd ,dir))
       (setq ,retval-var (funcall (lambda () ,@body)))
       (cd ,original-dir-var)
       ,retval-var)))

(defun spiffy-ruby-rerun-last-test ()
  (interactive)
  (save-buffer)
  (spiffy-run-in-directory
   spiffy-ruby-last-test-dir
   (compile spiffy-ruby-last-test-command)))

(defun spiffy-ruby-run-spec-under-point ()
  (interactive)
  (spiffy-ruby-run-spec
   (spiffy-local-file-name)
   "-c"
   "-fs"
   "--backtrace"
   "-l"
   (format "%d" (line-number-at-pos)))) ; defaults to line number at point

(defun spiffy-ruby-run-spec-file ()
  (interactive)
  (spiffy-ruby-run-spec (spiffy-local-file-name) "-c" "-fs"))

(defun spiffy-ruby-run-spec (specfile &rest spec-args)
  (save-buffer)
  (spiffy-run-in-directory
   (setq spiffy-ruby-last-test-dir (spiffy-ruby-bundle-root-for specfile))
   (compile (setq spiffy-ruby-last-test-command
                  ;; don't shell-escape "bundle exec spec"; it doesn't help
                  (concat (spiffy-ruby-maybe-bundled-command (buffer-file-name)
                                                             (spiffy-detect-rspec-binary)
                                                             (apply 'spiffy-make-shell-command
                                                                    (append spec-args (list specfile)))))))))

(defun spiffy-detect-rspec-binary ()
  (defvar cmd "bundle exec gem list | grep '^rspec ' | grep '(2'")
  (if (eql (call-process-shell-command cmd) 0) "rspec" "spec"))

(defun spiffy-ruby-maybe-bundled-command (filename program &optional args)
  (let ((bundle-root (spiffy-ruby-bundle-root-for filename))
        (space-and-args (if args
                            (concat " " args)
                          "")))
    (if bundle-root
        (concat "bash -l -c 'cd "
                bundle-root
                " && bundle exec "
                program
                space-and-args
                "'")
      (concat program
              space-and-args))))

(defun spiffy-ruby-bundle-root-for (filename)
  (let ((root (locate-dominating-file filename "Gemfile")))
    (if root
        (expand-file-name root)
      root)))

(defun spiffy-make-shell-command (&rest parts)
  (mapconcat 'shell-quote-argument parts " "))
