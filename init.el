(require 'hippie-exp)
(require 'package)

(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(ansi-color
                      centered-cursor-mode
                      cl-lib
                      coffee-mode
                      elisp-slime-nav
                      find-file-in-project
                      flymake
                      flymake-easy
                      flymake-haml
                      flymake-jshint
                      flymake-jslint
                      flyspell-lazy
                      highline
                      idle-highlight
                      idle-highlight-mode
                      ido-ubiquitous
                      inf-ruby
                      magit
                      mv-shell
                      paredit
                      projectile
                      projectile-rails
                      rainbow-mode
                      save-visited-files
                      scratch-persist
                      smex
                      solarized-theme
                      starter-kit
                      starter-kit-lisp
                      starter-kit-ruby
                      use-package
                      ws-trim
                      yaml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "54d1bcf3fcf758af4812f98eb53b5d767f897442753e1aa468cfeb221f8734f9" default)))
 '(enh-ruby-add-encoding-comment-on-save nil)
 '(grep-find-ignored-directories nil)
 '(grep-find-ignored-files nil)
 '(grep-find-template "find . <X> -type f <F> | xargs grep <C> -nH -e <R>")
 '(grep-highlight-matches (quote always))
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (flycheck eruby-mode hideshow-org hideshowvis swift-mode rjsx-mode web-mode js3-mode ruby-compilation flymake-ruby enh-ruby-mode rspec-mode bug-hunter use-package flycheck-swift3 swift3-mode transpose-frame rubocop python-mode php-mode save-visited-files scratch-persist immortal-scratch clojure-mode projectile projectile-rails yaml-mode ws-trim web starter-kit-ruby starter-kit-lisp solarized-theme rvm rainbow-mode pivotal-tracker mv-shell jenkins-watch idle-highlight highline haml-mode flyspell-lazy flymake-jslint flymake-jshint flymake-haml coffee-mode centered-cursor-mode bm)))
 '(pivotal-api-token "48e9258e1251ab9d802017b1bcb412cf")
 '(projectile-globally-ignored-directories
   (quote
    (".git" "log" "public/assets" "tmp" "spec/support/vcr_cassettes" "public/uploads/tmp")))
 '(projectile-globally-ignored-files
   (quote
    ("TAGS" ".rspec_history" ".byebug_history" "quickbooks-sandbox.md")))
 '(ruby-deep-indent-paren nil)
 '(save-visited-files-mode t)
 '(solarized-broken-srgb t)
 '(solarized-termcolors 16)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-fringe-persistent-face ((t (:background "orange1" :foreground "Black"))))
 '(bm-persistent-face ((t (:background "orange1" :foreground "black"))))
 '(erm-syn-errline ((t (:underline (:style wave :color "red")))))
 '(erm-syn-warnline ((t (:underline (:style wave :color "orange")))))
 '(flymake-errline ((((class color)) (:background "#ffffd7"))) t)
 '(flymake-warnline ((((class color)) (:background "#0a2832"))) t)
 '(grep-context-face ((t (:foreground "#839496"))) t)
 '(smerge-base ((t (:background "LightGoldenrod2"))))
 '(smerge-mine ((t (:background "dark red"))) t)
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "dark green")))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;;
;;;; Piotr's Custom Customs
;;;;

;;;; rvm stuff
(rvm-use-default)

;;;; set srgb
(setq ns-use-srgb-colorspace t)

;;;; new fullscreen emulating old fullscreen in Emacs 24.3
(setq ns-use-native-fullscreen nil)

;; fullscreen on Meta-Shift-f
(global-set-key (kbd "M-F") 'toggle-frame-fullscreen)

;;;; set column width to 80 unless someone sets higher in a mode
(setq-default fill-column 120)

;;;; set line hl-line-mode to true
(hl-line-mode t)

;; cleanup whitespace in a file
(global-set-key (kbd "C-x w c") 'whitespace-cleanup)

(setq-default show-trailing-whitespace t)
;; make the machine do the work to save me miles on finger strain
(add-hook 'before-save-hook #'whitespace-cleanup)

;; disable visual bell
(setq visible-bell nil)

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
(when window-system (global-unset-key (kbd "C-z")))

;; C-k will kill the whole line
(setq kill-whole-line t)

;; Don't jump to the beginning of a line when moving down to the next line
(setq line-move-visual t)

;; less typey typey
(defalias 'yes-or-no-p 'y-or-n-p)

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

;; window switch shortcuts
(global-set-key (kbd "M-N") (lambda ()
                            (interactive)
                            (next-multiframe-window)))

(global-set-key (kbd "M-P") (lambda ()
                            (interactive)
                            (previous-multiframe-window)))


(use-package rjsx-mode
  :ensure t
  :demand t

  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.json\\'" . rjsx-mode)
         ("\\.jsx\\'" . rjsx-mode))
  )

(use-package web-mode
  :ensure t
  :demand t

  :mode (("\\.html\\'" . web-mode))
  )

(use-package bm
  :ensure t
  :demand t

  :init
  ;; repository should be restored when loading `bm'
  (setq bm-restore-repository-on-load t)

  :config
  ;; look across all buffers when cycling through bookmarks
  (setq bm-cycle-all-buffers t)

  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")

  ;; make bookmarks persistent as default
  (setq-default bm-buffer-persistence t)

  ;; loading the repository from file when on start up
  (add-hook 'after-init-hook 'bm-repository-load)

  ;; restoring bookmarks when on file find
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; saving bookmark data on killing a buffer
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; saving the repository to file when on exit
  ;; `kill-buffer-hook' is not called when emacs is killed, so we
  ;; must save all bookmarks first
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))

  ;; update bookmark repository when saving the file
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; highligh mode for bookmarks
  (setq bm-highlight-style 'bm-highlight-line-and-fringe)

  ;; buffer should be recentered around the bookmark
  (setq bm-recenter t)

  :bind (("\C-x l" . bm-show-all)
         ("M-<f2>" . bm-toggle)
         ("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)))

;; ;; RUBY STUFF
(use-package enh-ruby-mode
  :ensure t
  :defer t

  :config
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-add-encoding-comment-on-save nil)

  :mode (("\\.rb\\'" . enh-ruby-mode)
         ("\\.ru\\'" . enh-ruby-mode)
         ("\\.gemspec\\'" . enh-ruby-mode)
         ("Rakefile\\'" . enh-ruby-mode)
         ("Gemfile\\'" . enh-ruby-mode)
         ("Capfile\\'" . enh-ruby-mode)
         ("Guardfile\\'" . enh-ruby-mode)))

(use-package flycheck
  :ensure t
  :defer t

  :init
  (add-hook 'enh-ruby-mode-hook 'flycheck-mode)
)

(use-package web-mode
  :ensure t
  :defer t

  :mode (("\\.erb\\'" . web-mode)))

(use-package rvm
  :ensure t
  :defer t)

(use-package rspec-mode
  :ensure t
  :defer t

  :init
  (add-hook 'enh-ruby-mode-hook 'rspec-mode)
  (add-hook 'enh-ruby-mode-hook 'hs-minor-mode)

  :config
  (setq rspec-use-rvm t)

  :bind (("M-R" . rspec-verify-single)
         ("M-r" . rspec-verify)
         ("C-?" . rspec-rerun)))

(use-package rubocop
  :ensure t
  :defer t

  :init (add-hook 'enh-ruby-mode-hook #'rubocop-mode))

(use-package hideshow
  :ensure t
  :bind ("C-c h" . hs-toggle-hiding)
  :config
  '(add-to-list 'hs-special-modes-alist
                `(enh-ruby-mode
                  ,(rx (or "def" "class" "it" "describe" "context" "module" "do" "{" "[")) ; Block start
                  ,(rx (or "}" "]" "end"))                       ; Block end
                  ,(rx (or "#" "=begin"))                        ; Comment start
		  ruby-forward-sexp nil)))

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

(global-set-key [(f5)] 'projectile-grep)
(global-set-key [(f6)] 'next-error)
(global-set-key [(shift f6)] 'previous-error)
(global-set-key "\C-x\C-b" 'buffer-menu)
(put 'set-goal-column 'disabled nil)
