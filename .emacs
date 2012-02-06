;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(require 'site-gentoo)
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;Haskell mode
(add-to-list 'load-path "~/.emacs.d/manual/ghc-mod")
(add-to-list 'exec-path "~/.cabal/bin")
(autoload 'ghc-init "ghc" nil t)

(require 'recentf)

;;Spell-check configuration
(setq ispell-program-name "aspell")
(setq ispell-local-dictionary "ukrainian")
(setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-list-command "list")

;;GUI settings
(setq scroll-bar-mode-explicit t) 
(set-scroll-bar-mode `right) 
(tool-bar-mode -1)

;;octave mode setup
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;;SLIME configuration
(setq slime-default-lisp "sbcl")
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
(setq inferior-lisp-program "/usr/bin/sbcl") 
(setq common-lisp-hyperspec-root "file:///home/local/Documents/books/informatics/lisp/HyperSpec/")
(setq slime-net-coding-system 'utf-8-unix)
(when (load "slime-autoloads" t)
  (setq slime-auto-connect 'always)
  (slime-setup '(slime-fancy slime-asdf inferior-slime))
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol))
(add-hook 'slime-connected-hook (lambda nil (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))
(add-hook 'slime-mode-hook (lambda nil (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

;;Setup for root-open
(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
	 ;; use a separate history list for "root" files.
	 (file-name-history find-file-root-history)
	 (name (or buffer-file-name default-directory))
	 (tramp (and (tramp-tramp-file-p name)
		     (tramp-dissect-file-name name)))
	 path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
	    dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(defun find-file-root-eval (name)
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."
  (require 'tramp)
  (let* ((file-name-history find-file-root-history)
	 (tramp (and (tramp-tramp-file-p name)
		     (tramp-dissect-file-name name)))
	 path dir file)
    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
	    dir (file-name-directory path)))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook)))

(global-set-key [(control x) (control r)] 'find-file-root)
;;END Setup for root-open

;;Bind useful keys
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "<XF86WakeUp>") 'ignore)
(global-set-key (kbd "C-x <XF86Display>") 'magit-status)
(global-set-key (kbd "C-x w") 
		(lambda () (interactive) 
		  (if (equalp 1 (length (window-list)))
		      (delete-frame)
		      (delete-window (selected-window)))))

;;DEFCUSTOMS
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-engine (quote xetex))
 '(anything-su-or-sudo "sudo")
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "chromium")
 '(browse-url-xterm-program "urxvt")
 '(comint-prompt-read-only t)
 '(default-input-method "ukrainian-computer")
 '(haskell-font-lock-symbols nil)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation turn-on-font-lock turn-on-haskell-doc-mode turn-on-haskell-decl-scan (lambda nil (ghc-init)))))
 '(inhibit-startup-screen t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(python-shell-extra-pythonpaths (quote ("/home/local/Documents/programming/python/scikit-learn")))
 '(quack-global-menu-p nil)
 '(quack-programs (quote ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-run-scheme-always-prompts-p nil)
 '(safe-local-variable-values (quote ((outline-minor-mode))))
 '(scroll-bar-mode (quote right))
 '(slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))
 '(tool-bar-mode nil)
 '(url-proxy-services nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "Liberation Mono"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "grey75" :foreground "black" :box (:line-width -1 :color "red3")))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "red3"))))))

;;Anything setup
(require 'anything-config)
(global-set-key (kbd "C-x b")
  (lambda() (interactive)
    (anything
     :prompt "Switch to: "
     :candidate-number-limit 10                 ;; up to 10 of each 
     :sources
     '( anything-c-source-buffers               ;; buffers 
        anything-c-source-recentf               ;; recent files 
        anything-c-source-bookmarks             ;; bookmarks
        anything-c-source-files-in-current-dir+)))) ;; current dir
;;Anything-R
(add-to-list 'load-path "~/.emacs.d/manual")
(require 'anything-R)
(add-hook 'ess-mode-hook (lambda () (local-set-key (kbd "C-c C-m") `anything-for-R)))

;;python setup
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
(setq ipython-command "ipython")
(require 'ipython)
(require 'anything-ipython)
(require 'python)
(setq python-check-command "pyflakes")
(setq
 python-shell-interpreter "ipython"
 py-python-command-args '("-colors" "LightBG")
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(__IP.complete('''%s'''))\n")

(define-key python-mode-map (kbd "M-TAB") 'rope-code-assist)
(define-key inferior-python-mode-map (kbd "M-TAB") 'rope-code-assist)


(defun TeX-math-insert-simple ()
  (interactive)
  (insert "$  $")
  (backward-char 2)
  (if current-input-method 
      (toggle-input-method)))

(defun TeX-math-insert ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (let ((a (region-beginning)) (b (region-end)))
	(kill-region a b)
	(insert "$ ")
	(yank)
	(insert " $")
	(setq kill-ring (cdr kill-ring)))
    (TeX-math-insert-simple)))

(defun TeX-math-insert-large ()
  (interactive)
  (insert "\n$$  $$\n")
  (if current-input-method 
      (toggle-input-method)))
(defun TeX-customize-math ()
  (define-key TeX-mode-map (kbd "C-c f") 'TeX-math-insert)
  (define-key TeX-mode-map (kbd "C-c F") 'TeX-math-insert-large)
  (define-key LaTeX-math-mode-map (kbd "` _") 'LaTeX-math-frac))

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-customize-math)
(require 'bbcode-mode)


(require 'dbus)
(defun im-notify ()
  (dbus-send-signal :system dbus-service-emacs dbus-path-emacs dbus-service-emacs "imChanged"  (if current-input-method 1 0)))
(defun im-notify-delete ()
  (dbus-send-signal :system dbus-service-emacs dbus-path-emacs dbus-service-emacs "imChanged"  0))
(add-hook 'input-method-activate-hook 'im-notify)
(add-hook 'input-method-inactivate-hook 'im-notify-delete)
(load "~/.emacs.d/manual/jiggle.el")
(add-hook 'jiggle-buffer-switch-hook 'im-notify)
(add-hook 'window-configuration-change-hook 'im-notify)
(add-hook 'delete-frame-functions 'im-notify-delete)
(add-hook 'mouse-leave-buffer-hook 'im-notify)

(defun im-request-handler (id)
  ;(message "Im request received: %d" id)
  (im-notify))
(dbus-register-signal 
  :system nil "/org/awesome/im"
  "org.awesome.im" "imRequest" 'im-request-handler)