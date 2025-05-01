(qv/package markdown-mode)
(qv/package yasnippet)

(add-to-list 'load-path "~/.emacs.d/elpa/lsp-bridge")


(yas-global-mode 1)

(require 'lsp-bridge)
(global-lsp-bridge-mode)
