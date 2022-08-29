;; Change to dark background for proper emacs colors
(let ((frame-background-mode 'dark))
  (frame-set-background-mode nil))


;; Always allow y instead of yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable the splash screen.
(setq inhibit-startup-message t)

;; Stop emacs from making those # files
(setq auto-save-default nil)

;; Turn on column numbers.
(column-number-mode 1)

;; Turn off scroll bars, the tool bar, and the menu bar.
(dolist (mode '(menu-bar-mode scroll-bar-mode tool-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Turn off the visual bell.
(setq visible-bell nil)

;; Highlight matching brackets.
(show-paren-mode 1)

;; Enable line numbers always
(global-display-line-numbers-mode)

;; Always use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Add a space after line numbers
(setq linum-format "%d ")


(setq ring-bell-function 'ignore)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)


(add-hook 'ido-define-mode-map-hook 'ido-my-keys)

(ido-mode 1)


;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)





(when (not package-archive-contents)
  (package-refresh-contents))


;; Actually let's you tab through directories and to files
(setq  ido-cannot-complete-command 'ido-exit-minibuffer)

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

    ;; adds interactive do (autocompletion) everywhere it can))
    ido-completing-read+

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://wwnw.emacswiki.org/emacs/Smex
    smex

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; How I do my undo stuff
    key-chord

    ;; Enable clipboard support
    pbcopy

    ;; Fix path issues
    exec-path-from-shell


    ;; Greatest undo system ever
    undo-tree

    ;; Search for things
    ag

   ;; mwheel
    ))






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

(xterm-mouse-mode 1)

(require 'mwheel)
(global-set-key [mouse-4] 'mwheel-scroll)
(global-set-key [mouse-5] 'mwheel-scroll)


(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 1000
      scroll-preserve-screen-position 1)





(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(ido-ubiquitous-mode 1)



(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(global-undo-tree-mode 1)


(require 'key-chord)
(key-chord-define-global "bb" 'mode-line-other-buffer)
(key-chord-define-global "zz" 'undo-tree-undo)
(key-chord-define-global "ZZ" 'undo-tree-redo)
(key-chord-define-global "jj" 'imenu)
(key-chord-mode +1)




(turn-on-pbcopy)



(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))


(global-set-key (kbd "M-j") 'join-line)


(setq create-lockfiles nil)
;; save backups elsewhere
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(idris-mode undo-tree smex rainbow-delimiters pbcopy paredit key-chord ido-completing-read+ exec-path-from-shell clojure-mode-extra-font-locking cider ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(setq ido-auto-merge-work-directories-length -1)
