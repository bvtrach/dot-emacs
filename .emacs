;;Haskell-mode
(add-to-list 'load-path "/home/local/emacs-stuff/ghc-mod/")
(add-to-list 'exec-path "/home/local/.cabal/bin")
(autoload 'ghc-init "ghc" nil t)
(load "/home/local/emacs-stuff/haskell-mode-2.8.0/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)

;;Spell-check configuration
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-list-command "list")

;;BBCode-mode
(load "/home/local/emacs-stuff/bbcode-mode.el")
(require 'bbcode-mode )

;;save-opened-files
;(load "/home/local/emacs-stuff/save-visited-files.el")
;(require 'save-visited-files)
;(turn-on-save-visited-files-mode)

;;normal scrollbars
(setq scroll-bar-mode-explicit t) 
(set-scroll-bar-mode `right) 
(tool-bar-mode -1)

;;Lua mode
(load "/home/local/emacs-stuff/lua-mode.el")
(require 'lua-mode )
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
(add-hook 'lua-mode-hook 'hs-minor-mode)

;;Sr Speedbar
(load "~/emacs-stuff/sr-speedbar.el")
(require 'sr-speedbar)

;;Stupid tetris 
(setq tetris-score-file "~/.emacs.d/tetris-scores")

;;SLIME configuration
(setq slime-default-lisp "sbcl")
(setq inferior-lisp-program "/usr/bin/sbcl") 
(setq common-lisp-hyperspec-root "file:///home/local/Desktop/stuff/HyperSpec/")
(setq slime-net-coding-system 'utf-8-unix)

;;Bind C-z to undo
(global-set-key (kbd "C-z") 'undo)
;;Fonts Fix
(set-fontset-font "fontset-default" '(#x0000 . #xFFFFF) '("Consolas" . "unicode-bmp"))
(add-to-list 'default-frame-alist '(font . ":Consolas-10"))
(setq initial-frame-alist default-frame-alist)
(setq special-display-frame-alist default-frame-alist)

;;Clear buffer function
(defun clear-buffer()
  "Kill all text in current buffer"
  (interactive) 
  (clipboard-kill-region 1 (point-max)) 
  (begining-of-buffer))

;;Swap buffers function
(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this))
     (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    )
  )

;;Set frame title
(setq frame-title-format
  '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))

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

(global-set-key [(control x) (control r)] 'find-file-root)
;;END Setup for root-open

;;DO NOT EDIT!
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-firefox-program "iceweasel")
 '(browse-url-generic-program "x-www-browser")
 '(column-number-mode t)
 '(comint-prompt-read-only t)
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(haskell-font-lock-symbols (quote unicode))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation turn-on-font-lock turn-on-haskell-doc-mode turn-on-haskell-decl-scan (lambda nil (ghc-init)))) t)
 '(inferior-haskell-wait-and-jump nil)
 '(menu-bar-mode t)
 '(org-directory "~/Desktop/org")
 '(org-tags-column 69)
 '(speedbar-show-unknown-files t)
 '(sr-speedbar-max-width 30)
 '(user-mail-address "bv.trach@gmail.com"))
 ;;(desktop-save-mode 1)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "microsoft" :family "Consolas"))))
 '(fringe ((t (:background "grey85"))))
 '(mode-line ((t (:background "grey80" :foreground "black" :box (:line-width -1 :color "red3")))))
 '(tooltip ((((class color)) (:inherit variable-pitch :background "#f5f5b5" :foreground "black" :height 100 :family "Droid Sans")))))

(when (load "slime-autoloads" t)
  (setq slime-auto-connect 'always)
  (slime-setup '(slime-fancy slime-asdf inferior-slime)))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
