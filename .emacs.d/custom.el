(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/journals/org/mylife.org" "~/journals/org/private.org" "~/journals/org/work.org"))
 '(org-capture-templates
   '(("p" "Private templates")
     ("pt" "TODO entry" entry
      (file+headline "~/journals/org/private.org" "Capture")
      (file "~/journals/org/tpl-todo.txt"))))
 '(org-log-into-drawer "CLOCKING")
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit org-id ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(package-selected-packages
   '(org-present org-noter org-pdfview pdf-tools accent use-package-chords bash-completion orgalist multiple-cursors yaml-mode vertico use-package typescript-mode rust-mode php-mode markdown-mode magit json-mode hledger-mode haskell-mode go-mode exec-path-from-shell elixir-mode eglot editorconfig doom-themes diff-hl corfu consult company auctex apheleia)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
