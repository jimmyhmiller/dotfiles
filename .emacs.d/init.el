;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)





;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)


(global-undo-tree-mode 1)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "5e52ce58f51827619d27131be3e3936593c9c7f9f9f9d6b33227be6331bf9881" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (aggressive-indent wgrep-ag wgrep robe rubocop rvm rjsx-mode ag haskell-emacs pbcopy nodejs-repl markdown-preview-mode markdown-mode+ idris-mode indium neotree fiplr key-chord inf-ruby which-key clj-refactor zenburn-theme zenburn undo-tree web-mode tagedit solarized-theme smex rainbow-delimiters projectile paredit magit ido-ubiquitous exec-path-from-shell clojure-mode-extra-font-locking cider ac-emacs-eclim))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :height 140 :width normal :foundry "nil" :family "Menlo")))))

(add-to-list 'default-frame-alist '(font . "Ubunto Mono" ))


(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))


(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


(global-set-key (kbd "C-x f") 'fiplr-find-file)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(load-theme 'solarized-dark t)



(require 'cider)


(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")



(require 'key-chord)
(key-chord-define-global "bb" 'mode-line-other-buffer)
(key-chord-define-global "zz" 'undo-tree-undo)
(key-chord-define-global "ZZ" 'undo-tree-redo)
(key-chord-mode +1)


(global-set-key (kbd "C-x f") 'fiplr-find-file)

(global-set-key (kbd "s-F") 'rgrep)


(xterm-mouse-mode 1)

(setq markdown-command "multimarkdown")


(defun second-last (list)
  (nth (- (length list) 2) list))

(defun display-eval-expr-js ()
  "Execute a command and output the result to the temporary buffer."
  (interactive)
  (let* ((start (save-excursion (nodejs-repl--beginning-of-expression)))
        (end (point))
        (command (buffer-substring start end))
        (ret (nodejs-repl--send-string (concat command "\n"))))
    (setq ret (replace-regexp-in-string nodejs-repl-ansi-color-sequence-re "" ret))
    (setq ret (replace-regexp-in-string "\\(\\w\\|\\W\\)+\r\r\n" "" ret))
    (setq ret (replace-regexp-in-string "\r" "" ret))
    (print (split-string ret "\[1G\[0J> \[3G"))
    (print "end \n")
    (setq ret (second-last (split-string (string-trim ret) "\n")))
    (cider--display-interactive-eval-result ret (point))))
(add-hook 'js-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "C-c C-e") 'display-eval-expr-js)
            (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
            (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
            (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))

(turn-on-pbcopy)


(setq-default neo-window-width 60)


(require 'mouse) ;; needed for iterm2 compatibility
(xterm-mouse-mode t)
(global-set-key [mouse-4] '(lambda ()
                           (interactive)
                           (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
                           (interactive)
                           (scroll-up 1)))
(setq mouse-sel-mode t)
(defun track-mouse (e))








(require 'dash)

(defcustom unity-repl-command "ruby repl-client.rb"
  "Command to use for arcadia-repl.")

(defcustom unity-repl-command-path "Assets/Arcadia/Editor"
  "Launch the REPL command in this relative path.")

(defun unity-root-p (dir)
  "Is this DIR the root of a Unity project?"
  (-any? (lambda (f)
           ;; TODO: Maybe this could be better?
           (string-equal f "ProjectSettings"))
         (directory-files dir)))

(defun unity-find-root (start levels)
  "Look upwards from the START directory to find the Unity root directory 
and return its full path. Search for the number of LEVELS specified."
  (cond ((= levels 0) nil)
        ((unity-root-p start) start)
        (t (unity-find-root
            (expand-file-name ".." start) (- levels 1)))))

(defun unity-jack-in ()
  "Start the Arcadia REPL"
  (interactive)
  (let ((default-directory
          (concat (unity-find-root default-directory 10) "/"
                  unity-repl-command-path "/")))
    (run-lisp unity-repl-command)))

(provide 'arcadia)


(setq linum-format "%d ")
