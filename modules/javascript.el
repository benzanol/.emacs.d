(bz/package web-mode)
(bz/require treesitter)

(add-hook 'js-mode-hook 'tree-sitter-mode)
(add-hook 'js-mode-hook 'tree-sitter-hl-mode)
;; (add-hook 'js-mode-hook 'web-mode)

(setq web-mode-enable-auto-quoting nil)

(bz/hook js-jsx-mode-hook bz/js-jsx-mode-setup
  (setcdr js-jsx-mode-map nil))
