(setq lsp-keymap-prefix "C-c")

(qv/package lsp-mode)
(qv/package lsp-ui)

(qv/require company)

(setcdr lsp-mode-map nil)

(qv/hook lsp-mode-hook qv/lsp-setup
  (setq lsp-headerline-breadcrumb-mode nil)
  (setcdr lsp-mode-map nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-enable-indentation nil)
  (lsp-headerline-breadcumbs-mode 0)

  ;; Enable company with immediate autocompleting
  (company-mode 1)
  (setq company-minimum-prefix-length 1)

  ;; Disable diagnostics in the modeline
  (lsp-modeline-diagnostics-mode 0))
(qv/lsp-setup)
