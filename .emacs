;;------------------------;;
; MELPA PACKAGE MANAGEMENT ;
;;------------------------;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;----------------------;;
; SET STARTUP PARAMETERS ;
;;----------------------;;

(setq initial-scratch-message "")
(ido-mode t)                   ; turn on ido mode
;(global-linum-mode t)        ; turn on line number mode
(setq make-backup-files nil)   ; turn off damn tilde files
(setq inhibit-splash-screen t) ; turn off splash screen
;(setq mouse-autoselect-window t)
;(normal-erase-is-backspace-mode 1)  ; needed for backspace to work in docker

(global-undo-tree-mode)

(require 'powerline)
(powerline-default-theme)

(defun select-next-window()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))
(global-set-key (kbd "M-`") 'select-next-window)

;;--;;
; GO ;
;;--;;
;; gofmt
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'before-save-hook #'gofmt-before-save)

;; linting
;; (add-to-list 'load-path "/home/lkirk/repo/godev/src/github.com/golang/lint/misc/emacs")
;; (require 'golint)

;; (setenv "GOPATH" "/home/lkirk/repo/godev")

;; autocompletion
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;;----------------------------;;
;  COMMENT REGION KEY COMMAND ;
;;---------------------------;;

(global-set-key (kbd "C-x c") 'comment-region)
(global-set-key (kbd "C-x C") 'uncomment-region)

;;---------;;
; EVIL MODE ;
;;---------;;
(require 'evil)
(evil-mode 1)
(evil-set-undo-system 'undo-tree)

;;-------------;;
; MARKDOWN MODE ;
;;-------------;;

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;---------------------------;;
; JEDI (COMPLETION FOR PYTHON ;
;;---------------------------;;

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

;;-----------------------;;
; EMACS SPEAKS STATISTICS ;
;;-----------------------;;

;;(require 'ess-site)

;;------------------------;;
; SERVER SHUTDOWN FUNCTION ;
;;------------------------;;

(defun ss ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;;--------;;
; FLYCHECK ;
;;--------;;

(global-flycheck-mode)
;;(setq flycheck-python-flake8-executable "flake8")
;;(setq flycheck-python-pylint-executable "pylint")
;; (flycheck-disable-checker "python-pylint")

(add-hook 'python-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(progn
     (define-key flycheck-mode-map (kbd "C-c n") 'flycheck-next-error)
     (define-key flycheck-mode-map (kbd "C-c N") 'flycheck-previous-error)
     ))

;; (eval-after-load 'flycheck
(add-hook 'python-mode-hook
  (lambda ()
     (let (ve-path)
       (dolist (env (frame-parameter nil 'environment) ve-path)
	 (if (string-prefix-p "VIRTUAL_ENV=" env)
	     (setq ve-path (cadr (split-string env "=" t)))))
       (if ve-path
	   (setq flycheck-python-pylint-executable (concat (file-name-as-directory ve-path) "bin/pylint"))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#7ec98f")
 '(cua-normal-cursor-color "#7c7c7c")
 '(cua-overwrite-cursor-color "#e5c06d")
 '(cua-read-only-cursor-color "#8ac6f2")
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(highlight-changes-colors '("#e5786d" "#834c98"))
 '(highlight-symbol-colors
   '("#3d173b6d14be" "#1afe3bcb3b84" "#3d173b6d14be" "#3d2217163bd6" "#1afe3bcb3b84" "#3d173b6d14be" "#1afe3bcb3b84"))
 '(highlight-symbol-foreground-color "#8b8b8b")
 '(highlight-tail-colors
   '(("#0b0b0b" . 0)
     ("#183130" . 20)
     ("#183130" . 30)
     ("#183130" . 50)
     ("#323013" . 60)
     ("#323013" . 70)
     ("#341307" . 85)
     ("#0b0b0b" . 100)))
 '(hl-bg-colors
   '("#323013" "#323013" "#323013" "#341307" "#321531" "#183130" "#183130" "#183130"))
 '(hl-fg-colors
   '("#000000" "#000000" "#000000" "#000000" "#000000" "#000000" "#000000" "#000000"))
 '(hl-paren-colors '("#7ec98f" "#e5c06d" "#a4b5e6" "#834c98" "#8ac6f2"))
 '(lsp-ui-doc-border "#8b8b8b")
 '(menu-bar-mode nil)
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#0b0b0b")
 '(pos-tip-foreground-color "#8b8b8b")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#8ac6f2" "#0b0b0b" 0.2))
 '(term-default-bg-color "#000000")
 '(term-default-fg-color "#7c7c7c")
 '(vc-annotate-background-mode nil)
 '(weechat-color-list
   '(unspecified "#000000" "#0b0b0b" "#323013" "#ffb4ac" "#183130" "#8ac6f2" "#323013" "#e5c06d" "#183130" "#a4b5e6" "#341307" "#e5786d" "#183130" "#7ec98f" "#7c7c7c" "#5e5e5e"))
 '(xterm-color-names
   ["#0b0b0b" "#ffb4ac" "#8ac6f2" "#e5c06d" "#a4b5e6" "#e5786d" "#7ec98f" "#eeeeee"])
 '(xterm-color-names-bright
   ["#000000" "#ddaa6f" "#525252" "#5e5e5e" "#7c7c7c" "#834c98" "#8b8b8b" "#ffffff"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
