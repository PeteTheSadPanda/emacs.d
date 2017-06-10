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
                      bm
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
                      flymake-ruby
                      flyspell-lazy
                      haml-mode
                      highline
                      idle-highlight
                      idle-highlight-mode
                      ido-ubiquitous
                      inf-ruby
                      jenkins-watch
                      magit
                      mv-shell
                      paredit
                      pivotal-tracker
                      projectile
                      projectile-rails
                      rainbow-mode
                      ruby-compilation
                      ruby-mode
                      rvm
                      save-visited-files
                      scratch-persist
                      smex
                      solarized-theme
                      starter-kit
                      starter-kit-lisp
                      starter-kit-ruby
                      web
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
 '(grep-find-ignored-directories (quote ("coverage" "log" "app/assets/javascripts/vendor")))
 '(grep-find-ignored-files
   (quote
    ("*.class" ".rspec_history" "app/assets/stylesheets/jquery.minicolors.css")))
 '(grep-find-template "find . <X> -type f <F> | xargs grep <C> -nH -e <R>")
 '(grep-highlight-matches (quote always))
 '(package-selected-packages
   (quote
    (flycheck-swift3 swift3-mode transpose-frame rubocop python-mode php-mode save-visited-files scratch-persist immortal-scratch clojure-mode projectile projectile-rails yaml-mode ws-trim web starter-kit-ruby starter-kit-lisp solarized-theme rvm ruby-compilation rainbow-mode pivotal-tracker mv-shell jenkins-watch idle-highlight highline haml-mode flyspell-lazy flymake-ruby flymake-jslint flymake-jshint flymake-haml coffee-mode centered-cursor-mode bm)))
 '(save-visited-files-mode t)
 '(solarized-broken-srgb t)
 '(solarized-termcolors 16))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-fringe-persistent-face ((t (:background "orange1" :foreground "Black"))))
 '(bm-persistent-face ((t (:background "orange1" :foreground "black"))))
 '(flymake-errline ((((class color)) (:background "#ffffd7"))))
 '(flymake-warnline ((((class color)) (:background "#0a2832"))))
 '(grep-context-face ((t (:foreground "#839496"))) t))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
