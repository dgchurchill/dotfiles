(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(aw-keys (quote (97 115 100 102 103 104 49 50 51 52 53 54)))
 '(aw-scope (quote frame))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups/"))))
 '(blink-cursor-blinks 0)
 '(company-box-icons-alist (quote company-box-icons-all-the-icons))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay 0)
 '(company-transformers (quote (company-sort-prefer-same-case-prefix)))
 '(custom-safe-themes
   (quote
    ("890a1a44aff08a726439b03c69ff210fe929f0eff846ccb85f78ee0e27c7b2ea" "f7216d3573e1bd2a2b47a2331f368b45e7b5182ddbe396d02b964b1ea5c5dc27" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(delete-by-moving-to-trash t)
 '(display-line-numbers (quote visual))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eglot-put-doc-in-help-buffer t)
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-prefer-doc-buffer t)
 '(evil-emacs-state-modes
   (quote
    (Custom-mode archive-mode bbdb-mode biblio-selection-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode mh-folder-mode monky-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode pdf-outline-buffer-mode pdf-view-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode ivy-occur-mode elfeed-show-mode elfeed-search-mode)))
 '(evil-magit-state (quote normal))
 '(evil-magit-use-y-for-yank t)
 '(evil-magit-want-horizontal-movement t)
 '(evil-respect-visual-line-mode t)
 '(fci-rule-color "#003f8e")
 '(flycheck-display-errors-function (quote flycheck-display-error-messages-unless-error-list))
 '(git-commit-summary-max-length 72)
 '(helm-mode-reverse-history nil)
 '(help-window-select t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote org-mode))
 '(initial-scratch-message nil)
 '(ivy-count-format "")
 '(ivy-fixed-height-minibuffer t)
 '(ivy-height 20)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(magit-completing-read-function (quote ivy-completing-read))
 '(magit-diff-expansion-threshold 5.0)
 '(magit-merge-arguments (quote ("--ff-only")))
 '(magit-rebase-arguments (quote ("--interactive")))
 '(magit-repository-directories
   (quote
    (("c:\\dev" . 2)
     ("C:\\Users\\Dave\\Documents\\Work" . 1)
     ("~/Projects" . 1))))
 '(menu-bar-mode nil)
 '(minions-direct (quote (purpose-mode)))
 '(neo-theme (quote icons))
 '(org-agenda-span (quote day))
 '(org-agenda-start-with-log-mode (quote (clock closed)))
 '(org-agenda-sticky t)
 '(org-babel-load-languages (quote ((calc . t) (emacs-lisp . t) (js . t))))
 '(org-catch-invisible-edits (quote error))
 '(org-clock-clocked-in-display (quote both))
 '(org-clock-into-drawer 1)
 '(org-clock-mode-line-total (quote current))
 '(org-html-postamble nil)
 '(org-image-actual-width (quote (800)))
 '(org-log-into-drawer t)
 '(org-src-fontify-natively t)
 '(package-selected-packages
   (quote
    (leuven-theme sql-indent go-mode php-mode eglot lsp org impatient-mode yaml-mode minions doom-themes company-box doom-modeline elpy elfeed all-the-icons neotree ivy-rich exec-path-from-shell flycheck window-purpose nord-theme fsharp-mode company gnuplot-mode ivy-hydra smex counsel projectile evil-magit tide web-mode typescript-mode powershell markdown-mode elm-mode which-key vlf use-package powerline magit gruvbox-theme evil csharp-mode ace-window)))
 '(projectile-completion-system (quote helm))
 '(rcirc-server-alist (quote (("irc.freenode.net"))))
 '(reb-re-syntax (quote string))
 '(scroll-bar-mode nil)
 '(scroll-margin 5)
 '(split-height-threshold 60)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(truncate-partial-width-windows nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff9da4")
     (40 . "#ffc58f")
     (60 . "#ffeead")
     (80 . "#d1f1a9")
     (100 . "#99ffff")
     (120 . "#bbdaff")
     (140 . "#ebbbff")
     (160 . "#ff9da4")
     (180 . "#ffc58f")
     (200 . "#ffeead")
     (220 . "#d1f1a9")
     (240 . "#99ffff")
     (260 . "#bbdaff")
     (280 . "#ebbbff")
     (300 . "#ff9da4")
     (320 . "#ffc58f")
     (340 . "#ffeead")
     (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil)
 '(w32-system-shells
   (quote
    ("cmd" "cmd.exe" "command" "command.com" "4nt" "4nt.exe" "4dos" "4dos.exe" "tcc" "tcc.exe" "ndos" "ndos.exe"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-fine-diff-A ((t (:box (:line-width 1 :style released-button)))))
 '(ediff-fine-diff-B ((t (:box (:line-width 1 :style released-button)))))
 '(ivy-highlight-face ((t nil))))
