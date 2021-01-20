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
(normal-erase-is-backspace-mode 1)  ; needed for backspace to work in docker

(global-undo-tree-mode)
;(evil-set-undo-system 'undo-tree)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes '(solarized-wombat-dark))
 '(custom-safe-themes
   '("13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(menu-bar-mode nil)
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
