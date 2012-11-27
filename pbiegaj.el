(require 'grep)
(require 'ansi-color)

;;;; set line hl-line-mode to true
(hl-line-mode t)

;;;; line numbers on the left in a gui
(global-linum-mode 1)

;; repository should be restored when loading `bm'
(setq bm-restore-repository-on-load t)

(add-to-list 'load-path "/Users/pbiegaj/.emacs.d/elpa/bm-1.53")
(require 'bm)

;; window switch shortcuts
(global-set-key "\M-N" 'next-multiframe-window)
(global-set-key "\M-P" 'previous-multiframe-window)

;; key binding
(global-set-key (kbd "<M-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

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
(add-hook' after-init-hook 'bm-repository-load)

;; restoring bookmarks when on file find
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save);

;; saving the repository to file when on exit
;; `kill-buffer-hook' is not called when emacs is killed, so we
;; must save all bookmarks first
(add-hook 'kill-emacs-hook '(lambda nil
                             (bm-buffer-save-all)
                             (bm-repository-save)))

;; update bookmark repository when saving the file
(add-hook 'after-save-hook 'bm-buffer-save)

;; auto update of modified files from the filesystem, mostly for
;; sharing two different editors on the same machine
(global-auto-revert-mode t)
;; don't iconify on C-z when running in X
;; or exit emacs (!) when running in Emacs.app
(when window-system (global-unset-key "\C-z"))

(setq kill-whole-line t)
(setq confirm-kill-emacs 'yes-or-no-p)

;; 4 Macs
(setq mac-command-modifier 'meta)

;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-color-theme-solarized")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-color-theme-solarized")

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
     (add-hook 'ruby-mode-hook 'flymake-ruby-load)
     (define-key ruby-mode-map [(meta r)] 'spiffy-ruby-run-spec-file)
     (define-key ruby-mode-map [(meta R)] 'spiffy-ruby-run-spec-under-point)
     (define-key ruby-mode-map [(control ?\;) ?r ?t] 'spiffy-ruby-rerun-last-test)))


;;;; spiffy-mode provides a few utilities
;; (add-to-list 'load-path "~/.emacs.d/vendor/spiffy")
;; (setq spiffy-enable-minor-mode t)
;; (require 'spiffy)

;;;; spiffy-textmate-mode has some good stuff, but I don't want the
;;;; full minor mode since it stomps all over a bunch of default keybindings
;; (require 'spiffy-textmate-mode)

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

(defadvice compilation-filter (before ansify-compilation-output activate)
  (with-current-buffer (process-buffer (ad-get-arg 0))
    (let ((colorstr (ansi-color-apply (ad-get-arg 1))))
      (ad-set-arg 1 (font-lock-proof colorstr 0)))))



(defvar *spiffy-ruby-keymap* (make-sparse-keymap) "Keybindings go in here")
(defun spiffy-ruby-define-key (key func)
  (define-key *spiffy-ruby-keymap* key func))

(global-set-key [(f5)] 'spiffy-tm-grep-project)
(global-set-key [(f6)] 'next-error)
(global-set-key [(shift f6)] 'previous-error)
(global-set-key "\C-x\C-b" 'buffer-menu)

(defun spiffy-find-interesting-files (directory interesting-p)
  (if (not (file-directory-p directory))
      (filter interesting-p (list directory))
    (append (filter interesting-p (list directory))
            (reduce 'append
                    (mapcar (lambda (dir) (spiffy-find-interesting-files dir interesting-p))
                            (filter interesting-p
                                    (mapcar (lambda (filename) (concat (file-name-as-directory directory) filename))
                                            (spiffy-useful-directory-files directory))))))))
(defun spiffy-parent-directory (filename)
  (file-name-as-directory (expand-file-name (concat(file-name-as-directory filename) ".."))))

(defun spiffy-tm-grep-project (regexp)
  "Search all the files in the current project for the specified string/regex."
  (interactive
   (list (grep-read-regexp)))
  (grep-compute-defaults)      ; rgrep only does this when called interactively
  (rgrep regexp "*" (spiffy-tm-project-root-for (buffer-file-name))))

(defun spiffy-tm-is-project-root (directory)
  (file-exists-p (concat (file-name-as-directory directory) ".git")))

(defun spiffy-tm-project-root-for (filename)
  (if (null filename)
      nil
    (let ((as-dir (file-name-as-directory filename)))
      (if (string= (file-truename as-dir) (file-truename (spiffy-parent-directory as-dir)))
          nil    ; base case
        (if (spiffy-tm-is-project-root as-dir)
            as-dir
          (spiffy-tm-project-root-for (spiffy-parent-directory filename)))))))

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
                                                              "spec"
                                                              (apply 'spiffy-make-shell-command
                                                                     (append spec-args (list specfile)))))))))

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
