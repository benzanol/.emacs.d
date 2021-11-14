;;; Copypasta
;; Enable scala-mode for highlighting, indentation and motion commands
(qv/package scala-mode)

;; Enable sbt mode for executing sbt commands
(qv/package sbt-mode)
;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;; allows using SPACE when in the minibuffer
(substitute-key-definition
 'minibuffer-complete-word
 'self-insert-command
 minibuffer-local-completion-map)
;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
(setq sbt:program-options '("-Dsbt.supershell=false"))

;; Enable nice rendering of diagnostics like compile errors.
(qv/package flycheck)

(add-hook 'lsp-mode 'lsp-lens-mode)

;; Uncomment following section if you would like to tune lsp-mode performance according to
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;       (setq gc-cons-threshold 100000000) ;; 100mb
;;       (setq read-process-output-max (* 1024 1024)) ;; 1mb
;;       (setq lsp-idle-delay 0.500)
;;       (setq lsp-log-io nil)
;;       (setq lsp-completion-provider :capf)
(setq lsp-prefer-flymake nil)

;; Add metals backend for lsp-mode
(qv/package lsp-metals)

;; Enable nice rendering of documentation on hover
;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;;   In that case you have to not only disable this but also remove from the packages since
;;   lsp-mode can activate it automatically.
(qv/package lsp-ui)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;;   to avoid odd behavior with snippets and indentation
(qv/package yasnippet)

;; Use company-capf as a completion provider.
;;
;; To Company-lsp users:
;;   Company-lsp is no longer maintained and has been removed from MELPA.
;;   Please migrate to company-capf.
(qv/package company)
(setq lsp-completion-provider :capf)

;; Use the Debug Adapter Protocol for running tests and debugging
(qv/package posframe)
;; Posframe is a pop-up tool that must be manually installed for dap-mode

(qv/package dap-mode)
(add-hook 'lsp-mode 'dap-mode)
(add-hook 'lsp-mode 'dap-ui-mode)

;;; Custom
(setq lsp-enable-symbol-highlighting nil)

;;; Running
(setq qv/scala-directory "~/Documents/Scala"
      qv/scala-project-name "Appolonian")

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(defun qv/scala-run ()
  (interactive)
  (shell-command
   (format "cd %s/%s/bin && fsc ../src/Main.sc* && scala Main &"
           (expand-file-name qv/scala-directory)
           qv/scala-project-name))
  (buffer-face-set 'fixed-pitch))

;;(qv/keys scala-mode-map
;;  "<f5>" qv/scala-run)

;;; Floating Windows
(qv/hook exwm-manage-finish-hook qv/float-scala-window
  (when (or (string= (car (split-string exwm-class-name "-")) "scala")
            (string= exwm-class-name "main-Main"))
    (let ((win (selected-window)))
      (exwm-floating-toggle-floating)
      (set-window-buffer win "*Async Shell Command*")
      (exwm-layout-hide-mode-line))))

;;; Refresh ScalaJS
(setq qv/scalajs-buffer-name ":ScalaJS:")

(qv/keys scala-mode-map
  "<f5>" qv/refresh-scalajs)

(defun qv/refresh-scalajs ()
  (interactive)
  (let* ((buf (current-buffer))
         (win (selected-window))
         (js-buf (get-buffer qv/scalajs-buffer-name))
         (js-win (and js-buf (get-buffer-window js-buf))))
    (unless js-buf (error "No buffer `%s`" qv/scalajs-buffer-name))
    (if js-win (select-window js-win)
      (switch-to-buffer js-buf))
    (sleep-for 0.2)
    (qv/run-in-background "xdotool key F5")
    (select-window win)
    (switch-to-buffer buf)))

;;; Setup
(qv/keys scala-mode-map
  "g =" (@ qv/scala-indent-document
           (save-excursion
             (dotimes (i (line-number-at-pos (point-max)))
               (goto-line (1+ i))
               (scala-indent:indent-line))))
  "<tab>" scala-indent:indent-line)

(qv/hook scala-mode-hook qv/scala-setup
  (lsp-headerline-breadcrumb-mode 0)
  (yas-minor-mode 1)
  (run-with-timer 0.01 nil (lambda () (flycheck-mode 0)))
  (setq-local indent-tabs-mode t
              tab-width 2))
