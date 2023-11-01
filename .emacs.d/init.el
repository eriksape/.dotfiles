;;; Personal configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;;(package-initialize)

;; variables
(defvar var/default-font-size 89)

;; Load a custom theme

;;(load-theme 'wombat t)
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes"))
(load-theme 'nord t)

;; Set Window properties

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set default font face
(set-face-attribute 'default nil :font "MesloLGS NF" :height var/default-font-size :weight 'regular :slant 'normal)

;; Add the NonGNU ELPA package archive
(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(unless package-archive-contents  (package-refresh-contents))

(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
(scroll-bar-mode -1)

;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; Load system environment
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(exec-path-from-shell-initialize)
;;(exec-path-from-shell-copy-env "PASSWORD_STORE_DIR")

;; password-store
(add-to-list 'load-path "~/.emacs.d/packages/password-store/contrib/emacs")
(require 'password-store)
(setq auth-source-pass-filename "~/.local/share/gopass/stores/root")

;;; Completion framework
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

;; Enable completion by narrowing
(vertico-mode t)

;; Improve directory navigation
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
  (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char))

;; completion in region
(unless (package-installed-p 'corfu)
  (package-install 'corfu))

(global-corfu-mode)
(setq corfu-auto t
      corfu-quit-at-boundary 'separator
      )
(setq completion-cycle-threshold 2)
(setq tab-always-indent 'complete)

;;; Extended completion utilities
(unless (package-installed-p 'consult)
  (package-install 'consult))
(global-set-key [rebind switch-to-buffer] #'consult-buffer)
(global-set-key (kbd "C-c j") #'consult-line)
(global-set-key (kbd "C-c i") #'consult-imenu)

;;; Git client
(unless (package-installed-p 'magit)
  (package-install 'magit))

;; Bind the `magit-status' command to a convenient key.
(global-set-key (kbd "C-c g") #'magit-status)

;;; Multiple cursors
(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))

;; Move text
(add-to-list 'load-path "~/.emacs.d/packages/move-text")
(require 'move-text)
(move-text-default-bindings)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Show word-granularity differences within diff hunks
(setq magit-diff-refine-hunk t)

;;; LSP Support
(unless (package-installed-p 'eglot)
  (package-install 'eglot))

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)

;; Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)

;;; Go Support
(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

;;; JSON Support
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

;;; Lua Support
(unless (package-installed-p 'lua-mode)
  (package-install 'lua-mode))

;;; PHP Support
(unless (package-installed-p 'php-mode)
  (package-install 'php-mode))

;;; Rust Support
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))

;;; Additional Lisp support
(unless (package-installed-p 'sly)
  (package-install 'sly))

;;; Typescript Support
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

;;; YAML Support
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

;;; LaTeX support
(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Enable LaTeX math support
(add-hook 'LaTeX-mode-map #'LaTeX-math-mode)

;; Enable reference mangment
(add-hook 'LaTeX-mode-map #'reftex-mode)

;;; Markdown support
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;;; Web mode
(unless (package-installed-p 'web-mode) (package-install 'web-mode))

;; TSX support
(with-eval-after-load 'eglot
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

;;; Outline-based notes management and organizer
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)

;;; Additional Org-mode related functionality
(unless (package-installed-p 'org-contrib)
  (package-install 'org-contrib))

;;; EditorConfig support
(unless (package-installed-p 'editorconfig) (package-install 'editorconfig))

;; Enable EditorConfig
(editorconfig-mode t)

;; Hledger Mode
(unless (package-installed-p 'popup) (package-install 'popup))
(add-to-list 'load-path "~/.emacs.d/packages/hledger-mode")
(require 'hledger-mode)
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
(setq hledger-jfile "~/journals/ledger/2023.journal")
(setq hledger-currency-string "$")

(defun hledger-completion-accounts ()
  (when-let ((bounds (and (boundp 'hledger-accounts-cache)
                          (bounds-of-thing-at-point 'symbol))))
    (list (car bounds) (point) hledger-accounts-cache)))

(add-hook 'hledger-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions 'hledger-completion-accounts)))

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
