;; Set Colors
(setq
 qv/fg-color     "#B0C0CC"
 qv/bg-color     "#1B1F26"
 qv/bg2-color    "#14161B"
 qv/bg3-color    "#1F252E"
 qv/gray1-color  "#A4A8AC"
 qv/gray2-color  "#7C8084"
 qv/gray3-color  "#484B54"
 qv/black-color  "#0E1216"
 qv/red-color    "#D75F5F"
 qv/yellow-color "#FFD75F"
 qv/orange-color "#FFA500"
 qv/green-color  "#40E040"
 qv/cyan-color   "#5FFFD7"
 qv/blue-color   "#5FAFD7"
 qv/purple-color "#AF87D7")

(unless (display-graphic-p)
  (setq
   qv/bg-color     "#000000"
   qv/bg2-color    "#111111"
   qv/bg3-color    "#000000"
   qv/black-color  "#000000"))

;; Set Environment
(setq
 qv/environment
 '((functions
    (run-shell-command t)
    (face-function t))
   (packages
    (enable t)
    (use-package t)
    (evil
     (enable t)
     (global-movements t)
     (tetra-movements t)
     (maximum-movements t)
     (word-movements t)
     (one-key-deletions t)
     (custom-motions t)
     (manual-indenting t)
     (dynamic-indenting t)
     (deleting-without-saving t)
     (meta-q-to-quit t)
     (custom-search-mappings t)
     (undo-tree t)
     (join-lines t)
     (run-selected-code t)
     (kill-selected-text t)
     (meta-x t))
    (dired
     (enable t)
     (icons t)
     (tree t)
     (git-status nil)
     (toggle-hidden t)
     (changing-directory t)
     (mappings t)
     (startup t)
     (reloading t))
    (helpful
     (enable t)
     (format-helpful-buffers t))
    (which-key t)
    (neotree nil)
    (ov nil)
    (ivy
     (enable t)
     (faces t)
     (custom-map t)
     (posframe t)
     (details nil)
     (prescient t)
     (counsel t))
    (embark t)
    (orderless t)
    (undo-tree t)
    (company t)
    (doom-modeline t)
    (rainbow-delimiters t))
   (settings
    (unmap-ctrl-z t)
    (caps-lock-control t)
    (display-buffer t)
    (display-time-mode "%h %d  %H:%M |")
    (theme-directory "~/.emacs.d/themes")
    (disable-gui-elements t)
    (disable-start-screen t)
    (subword-movements t)
    (enable-trash t)
    (enable-auto-saves t)
    (enable-lock-files nil)
    (enable-backup-files nil)
    (use-tabs nil)
    (tab-width 4)
    (show-line-numbers t)
    (enable-relative-window-resizing t)
    (recursive-minibuffers nil)
    (winner-mode t)
    (set-scroll-margin 1000000)
    (set-scroll-step 0)
    (stretch-cursor t)
    (fringe-width 10)
    (remember-recent-files t)
    (highlight-whitespace nil))
   (org
    (enable t)
    (bullet-overlay t)
    (hide-ellipsis t)
    (hide-emphasis-markers t)
    (small-empty-lines t)
    (suscripts t)
    (indent-nested-headings t)
    (variable-pitch-font t)
    (disable-line-numbers t)
    (search-headings t)
    (return-follows-links t)
    (toggle-folds t)
    (unmap-meta-h t)
    (equation-overlays t)
    (hide-meta-text t)
    (visual-column t)
    (heading-bullets t)
    (code-blocks t)
    (latex-fragments t)
    (equation-syntax t)
    (inserting-text t)
    (indenting-code-blocks t))
   (exwm
    (enable t)
    (force-char-mode t)
    (global-mappings t)
    (start-exwm t)
    (opacity t)
    (wallpaper t)
    (audio t)
    (brightness t)
    (xinput t)
    (trackpad t)
    (display-settings t))
   (programming
    (enable t)
    (outline-mode t)
    (elisp-setup t)
    (adding-emacs-settings t))
   (extra
    (basic-faces t)
    (run-shell-command t)
    (browse-kill-ring t)
    (change-text-size t)
    (custom-buffers t)
    (fuzzy-find t)
    (buffer-manager t)
    (moving-windows t)
    (org-connect t)
    (align-tables t)
    (prime-factorization t)
    (nix-packages nil))))
