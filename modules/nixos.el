;; -*- lexical-binding: t; -*-

(require 'bz-base)

(require 'dash)
(require 'ht)


(bz/keys *
  "C-x C-n" nil
  "C-x C-n C-r" bz/nixos-reload
  "C-x C-n C-c" bz/nixos-config
  "C-x C-n C-i" bz/nix-install)


;;; Edit config

(defun bz/nixos-config ()
  (interactive)
  (find-file "/sudo::/etc/nixos/configuration.nix"))

(defun bz/nixos-reload ()
  (interactive)
  (if (y-or-n-p "Switch? ")
      (shell-command "NIXPKGS_ALLOW_UNFREE=1 sudo nixos-rebuild switch &")
    (shell-command "NIXPKGS_ALLOW_UNFREE=1 sudo nixos-rebuild test &")))


;;; Nix package search

(defvar bz/nix-package-string nil)
(defvar bz/nix-packages nil)

(defun bz/nix-install ()
  (interactive)
  (unless bz/nix-packages
    (setq bz/nix-package-string
          ($$ "cat '/home/benzanol/Programs/nix-packages' | sed 's/ \\+[^ ]\\+ \\+/\t/'"))
    (setq bz/nix-packages
          (ht<-alist
           (--map (let ((split (split-string it "\t")))
                    (put-text-property 0 5 'face 'org-verbatim (car split))
                    (cons (car split) (cadr split)))
                  (--filter (> (length it) 5) (split-string bz/nix-package-string "\n"))))))

  (let ((package (completing-read "Package: " (ht-keys bz/nix-packages))))
    (message "Installing %s" package)
    (save-window-excursion
      (shell-command
       (format "NIXPKGS_ALLOW_UNFREE=1 nix profile install %s &"
               (replace-regexp-in-string "^nixos." "nixpkgs#" package))
       "*Nix Install*"))))

(defun bz/nix-package-annotator (candidate)
  (when-let ((desc (ht-get bz/nix-packages candidate)))
    (marginalia--fields
     (desc :truncate 1.0 :face 'marginalia-documentation))))

;; Add the annotator function to marginalia
(push '(bz/nix-install bz/nix-package-annotator builtin none)
      marginalia-annotator-registry)

;; Set bz/nix-install to use the correct annotator
(push '(bz/nix-install . bz/nix-install) marginalia-command-categories)


;;; Nix install
;; (defun nix-install (pkg)
;;   (interactive
;;    (list (completing-read
;;           "Install Package: "
;;           (--map (or (ignore-errors (substring it 27 -4)) it)
;;                  (cdr (split-string
;;                        ($$ "nix search | awk '/^\\*/{print $2}'")
;;                        "\n"))))))
;;   (save-window-excursion
;;     (shell-command
;;      (format "export NIXPKGS_ALLOW_UNFREE=1 ; nix-env -iA nixos.%s && echo Done &" pkg)
;;      "*Nix Install*")))


;;; Nix mode

(bz/package nix-mode)


;;; Provide

(provide 'bz-nixos)
