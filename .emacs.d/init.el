;;; Personal configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 130)
(defvar efs/default-variable-font-size 130)

;; Add Elpa Melpa packages
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Add the NonGNU ELPA package archive
(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

;; Load system environment

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PASSWORD_STORE_DIR"))

;; pinetry
(setq epg-pinentry-mode 'loopback)
(setq transient-default-level 5)


;; window
(set-frame-parameter (selected-frame) 'alpha '(94 . 94))
(add-to-list 'default-frame-alist '(alpha . (94 . 94)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Load font
;;(set-frame-font "Fira Mono 18" nil t)
(set-face-attribute 'default nil :font "MesloLGS NF" :height efs/default-font-size :weight 'regular :slant 'normal)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "MesloLGS NF" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "MesloLGS NF" :height efs/default-variable-font-size :weight 'bold :slant 'normal)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
(scroll-bar-mode -1)

;; Disable splash screen
(setq inhibit-startup-screen t)

;;; Completion framework
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

;; Enable completion by narrowing
(vertico-mode t)

;;; Extended completion utilities
(unless (package-installed-p 'consult)
  (package-install 'consult))
(global-set-key [rebind switch-to-buffer] #'consult-buffer)
(global-set-key (kbd "C-c j") #'consult-line)
(global-set-key (kbd "C-c i") #'consult-imenu)

;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Automatically pair parentheses
(electric-pair-mode t)

;;; LSP Support
(unless (package-installed-p 'eglot)
  (package-install 'eglot))

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)

;;; Inline static analysis

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;; Display messages when idle, without prompting
(setq help-at-pt-display-when-idle t)

;; Message navigation bindings
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error))

;;; Pop-up completion
;;(unless (package-installed-p 'corfu)
;;  (package-install 'corfu))

;; Enable autocompletion by default in programming buffers
;;(add-hook 'prog-mode-hook #'corfu-mode)

;; Enable automatic completion.
;;(setq corfu-auto t)


;; company
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)
(use-package company
  :ensure t
  :config (global-company-mode t))

;;; Git client
(unless (package-installed-p 'magit)
  (package-install 'magit))

;; Bind the `magit-status' command to a convenient key.
(global-set-key (kbd "C-c g") #'magit-status)

;;; Indication of local VCS changes
(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))

;; Enable `diff-hl' support by default in programming buffers
(add-hook 'prog-mode-hook #'diff-hl-mode)

;; Update the highlighting without saving
(diff-hl-flydiff-mode t)

;;; Elixir Support
(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))

;;; Go Support
(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

;;; Haskell Support
(unless (package-installed-p 'haskell-mode)
  (package-install 'haskell-mode))

;;; JSON Support
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

;;; PHP Support
(unless (package-installed-p 'php-mode)
  (package-install 'php-mode))

;;; Rust Support
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))

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

;;; EditorConfig support
(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))

;; Enable EditorConfig
(editorconfig-mode t)

;; Enable apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;; Doom theme
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-nord t))

;; hledger
(use-package hledger-mode :ensure t)
;; To open files with .journal extension in hledger-mode
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

;; Provide the path to you journal file.
;; The default location is too opinionated.
(setq hledger-jfile "~/journals/ledger/2023.journal")

(defvar hledger-account-regex2
  (concat "(?\\(\\([Rr]evenues?\\|[bB]udgets?\\|[aA]ssets?\\|[lL]iabilit\\(?:ies\\|y\\)\\|[Dd]ebts?"
	  "\\|[Ee]quity\\|[Ee]xpenses?\\|[iI]ncome\\|[Zz]adjustments?\\)" ;; terms all accounts must start with
	  "\\(:[A-Za-z0-9\-]+\\( [A-Za-z0-9\-]+\\)*\\)*\\))?"             ;; allow multi-word account names
	  )
  "Regular expression for a potential journal account.")

(setq hledger-whitespace-account-regex (format "\\s-*%s" hledger-account-regex2))


;;; Auto-completion for account names
;; For company-mode users,
(add-to-list 'company-backends 'hledger-company)

;; For auto-complete users,
;(add-to-list 'ac-modes 'hledger-mode)
(add-hook 'hledger-mode-hook
    (lambda ()
        (setq-local ac-sources '(hledger-ac-source))))
(setq hledger-currency-string "$")

;(add-hook 'outline-mode-hook 'hledger-mode)
;; (add-hook 'hledger-mode-hook 'outline-mode)
;;(font-lock-add-keywords 'gyo-highlight-numbers-mode '(("[0-9]+" . 'custom-faces-highlight-numbers-face)))

(eval-after-load 'hledger-mode
       (progn
         ;; org-cycle allows completion to work whereas outline-toggle-children does not
         (define-key hledger-mode-map (kbd "TAB") #'org-cycle)
         (add-hook 'hledger-mode-hook #'outline-minor-mode)
         (font-lock-add-keywords 'hledger-mode 'outline-font-lock-keywords)))

					;(setq hledger-extra-args " -I check -I mp -I cd -I cash -I liabilities -I budgets -t 2 -V")

(defun hledger-custom-clean-extra-args()
  (interactive)
  (setq hledger-extra-args " ")
  (prin1 hledger-extra-args))

(defun hledger-custom-extra-args()
  (interactive)
;;  (prin1 (string-match "-V" hledger-extra-args)))
  (if(string-match-p "-V" hledger-extra-args)
      (setq  hledger-extra-args " -I check -I mp -I cd -I cash -I liabilities -I budgets -t 2")
    (setq  hledger-extra-args " -I check -I mp -I cd -I cash -I liabilities -I budgets -t 2 -V"))
  (prin1 hledger-extra-args))

;;; Multiple cursors
(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))

;; Move text
(use-package move-text :ensure t)
(move-text-default-bindings)


(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

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

;; orgmode
;;(setq org-refile)
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-log-into-drawer t)
(setq org-log-reschedule (quote time))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f6>") 'org-capture)

(defun org-focus-private() "Set focus on private things."
       (interactive)
       (setq org-agenda-files '("~/org/private.org")))

(defun org-focus-work() "Set focus on work things."
       (interactive)
       (setq org-agenda-files '("~/org/work.org")))

(defun org-focus-all() "Set focus on all things."
       (interactive)
       (setq org-agenda-files '("~/org/work.org"
				"~/org/private.org")))
(setq org-clock-sound "~/.emacs.d/ding.wav")
(defun my/play-sound (orgin-fn sound)
  (cl-destructuring-bind (_ _ file) sound
    (make-process :name (concat "play-sound-" file)
                  :connection-type 'pipe
                  :command `("afplay" ,file))))
(advice-add 'play-sound :around 'my/play-sound)
(if (eq system-type 'darwin) (advice-add 'play-sound :around 'my/play-sound))


(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-fancy-priorities
  :diminish
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

(use-package org-pretty-tags
  :diminish org-pretty-tags-mode
  :ensure t
  :init (setq org-roam-v2-ack t)
  :config
  (setq org-pretty-tags-surrogate-strings
        '(("work"  . "⚒")))

  (org-pretty-tags-global-mode))

(use-package key-chord :ensure t)
(use-package use-package-chords :ensure t)

(use-package accent
  :ensure t
  :chords (("aa" . ar/spanish-accent-menu)
           ("ee" . ar/spanish-accent-menu)
           ("ii" . ar/spanish-accent-menu)
           ("oo" . ar/spanish-accent-menu)
           ("uu" . ar/spanish-accent-menu)
           ("AA" . ar/spanish-accent-menu)
           ("EE" . ar/spanish-accent-menu)
           ("II" . ar/spanish-accent-menu)
           ("OO" . ar/spanish-accent-menu)
           ("UU" . ar/spanish-accent-menu)
           ("nn" . ar/spanish-accent-menu)
           ("NN" . ar/spanish-accent-menu)
           ("??" . ar/spanish-accent-menu)
           ("!!" . ar/spanish-accent-menu))
  :config
  (defun ar/spanish-accent-menu ()
    (interactive)
    (let ((accent-diacritics
           '((a (á))
             (e (é))
             (i (í))
             (o (ó))
             (u (ú ü))
             (A (Á))
             (E (É))
             (I (Í))
             (O (Ó))
             (U (Ú Ü))
             (n (ñ))
             (N (Ñ))
             (\? (¿))
             (! (¡)))))
      (ignore-error quit
        (accent-menu)))))

;(use-package org-roam
;  :ensure t
;  :custom (org-roam-directorry "~/journals/sbrain")
;  :bind (("C-c n l" . org-roam-buffer-toggle)
;	 ("C-c n f" . org-roam-node-find)
;	 ("C-c n i" . org-roam-node-insert)
;	 :map org-mode-map ("C-M-i" . completion-at-point))
;  :config (org-roam-config))

;; pcomplete

(require 'bash-completion)
(bash-completion-setup)

(defun pcomplete/sudo ()
  "Completion rules for the `sudo' command."
  (let ((pcomplete-ignore-case t))
    (pcomplete-here (funcall pcomplete-command-completion-function))
    (while (pcomplete-here (pcomplete-entries)))))


;;;; man completion
(defvar pcomplete-man-user-commands
  (split-string
   (shell-command-to-string
    "apropos -s 1 .|while read -r a b; do echo \" $a\";done;"))
  "p-completion candidates for `man' command")

(defun pcomplete/man ()
  "Completion rules for the `man' command."
  (pcomplete-here pcomplete-man-user-commands))

(defconst pcmpl-gopass-commands
  '("show" "show -c")
  "List of `gopass' commands")

(defconst pcmpl-gopass-secrets
  (split-string
   (shell-command-to-string "gopass ls -f"))
  "List of `gopass' secrets.")

(defun pcomplete/gopass ()
  "Complete rules for `gopass` command."
  (pcomplete-here pcmpl-gopass-commands))
  


;; tittle
;; (setq frame-title-format
;;      `((buffer-file-name "%f" "%b")
;;        ,(format " - GNU Emacs %s" emacs-version)))

;; SHOW FILE PATH IN FRAME TITLE
(setq frame-title-format
  '(:eval
    (if buffer-file-name
        (replace-regexp-in-string
         "\\\\" "/"
         (replace-regexp-in-string
          (regexp-quote (getenv "HOME")) "~"
          (convert-standard-filename buffer-file-name)))
      (buffer-name))))

;; pass
;;(use-package pass :ensure t)
(use-package password-store :ensure t)
;;(setq  password-store-dir "~/.local/share/gopass/stores/root")
(setq auth-source-pass-filename "~/.local/share/gopass/stores/root")

;; pdf-tools
(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install)
  )

;(use-package org-pdfview :ensure t)

;; org-present

(use-package org-present :ensure t)
(use-package visual-fill-column :ensure t)

(defun my/org-present-start ()
  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun my/org-present-end ()
  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)

;; Configure fill width
(setq visual-fill-column-width 110
      visual-fill-column-center-text t)

;; org-babel

(setq org-plantuml-jar-path (expand-file-name "/usr/local/opt/plantuml/libexec/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(require 'ob-js)

(add-to-list 'org-babel-load-languages '(js . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js .t)
 (plantuml . t))
 )

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
