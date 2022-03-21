;;; Nix install
(defun nix-install (pkg)
  (interactive
   (list (completing-read
          "Install Package: "
          (--map (or (ignore-errors (substring it 27 -4)) it)
                 (cdr (split-string
                       (shell-command-to-string
                        "nix search | awk '/^\\*/{print $2}'")
                       "\n"))))))
  (save-window-excursion
    (shell-command
     (format "export NIXPKGS_ALLOW_UNFREE=1 ; nix-env -iA nixos.%s && echo Done &" pkg)
     "*Nix Install*")))

;;; Nix mode
(qv/package nix-mode)
