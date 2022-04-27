;;; package --- Summary:

;;; Commentary:
(require 'package)

;;; Code:

(setq package-list '(
		     evil
		     cyberpunk-theme
		     jedi
		     magit
		     dockerfile-mode
		     systemd
		     flycheck
		     bison-mode
		     blacken
		     clang-format
		     sphinx-doc
		     cmake-mode
		     csharp-mode
		     cython-mode
		     docker
		     go-autocomplete
		     jinja2-mode
		     jq-mode
		     nginx-mode
		     nix-mode
		     powerline-evil
		     rainbow-mode
		     rust-mode
		     solarized-theme
		     zenburn-theme
		     yaml-mode
		     undo-tree
		     ))

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
