(qv/package web-mode)
(qv/require treesitter)

(add-hook 'js-mode-hook 'tree-sitter-mode)
(add-hook 'js-mode-hook 'tree-sitter-hl-mode)
;;(add-hook 'js-mode-hook 'web-mode)

(setq web-mode-enable-auto-quoting nil)
