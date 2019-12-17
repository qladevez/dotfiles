;; Telling emacs to stop spreading files with ~ everywhere
(setq make-backup-files nil)

;; Disable emacs toolbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Disable emacs startup screens
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it on startup.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; HOL
;; (load "/home/qladevez/build/HOL/tools/hol-mode")

;; (defun my-sml-mode-hook ()
;; "Local defaults for SML mode"
;; (setq electric-indent-chars '()))

;; (add-hook 'sml-mode-hook 'my-sml-mode-hook)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; When some text is selected and I type, it replaces the text
(delete-selection-mode 1)

;; Set PolyML as my SML interpreter
;; (setq sml-program-name "poly")

;; Load stuff
(require 'ido)
(ido-mode t)

(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;; C configuration

(require 'cc-mode)
(setq-default c-basic-offset 4)
(setq c-default-style (cons '(c-mode . "java") c-default-style))
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(add-hook 'c-mode-hook
       (lambda ()
         (setq-local electric-indent-chars
                     (remq ?\n electric-indent-chars))))

(require 'telephone-line)
(set-face-attribute 'telephone-line-evil-normal
                    nil
		    :foreground "white"
                    :background "saddle brown")
(set-face-attribute 'telephone-line-evil-insert
                    nil
                    :foreground "white"
                    :background "royal blue")
(set-face-attribute 'telephone-line-evil-visual
                    nil
		    :foreground "white"
                    :background "dark violet")

(setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
(setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
(telephone-line-mode 1)

;; Evil configuration
(require 'evil)
(evil-mode 1)

;; Coq abbreviations are annoying
(setq evil-want-abbrev-expand-on-insert-exit nil)
(setq coq-mode-abbrev-table '())

(require 'evil-surround)
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "f" 'ido-find-file
  "<SPC>" 'save-buffer
  "b" 'ido-switch-buffer
  "<right>" 'next-buffer
  "<left>" 'previous-buffer
  "q" 'save-buffers-kill-terminal
  "x" 'execute-extended-command)

(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))

(electric-pair-mode 1)

;; OCaml
(require 'cl)

(load "/home/qladevez/.opam/default/share/emacs/site-lisp/tuareg-site-file")

(require 'tuareg)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode))
          auto-mode-alist))

;; -- opam and utop setup --------------------------------
;; Setup environment variables using opam
(dolist
   (var (car (read-from-string
           (shell-command-to-string "opam config env --sexp"))))
 (setenv (car var) (cadr var)))
;; Update the emacs path
(setq exec-path (split-string (getenv "PATH") path-separator))
;; Update the emacs load path
(push (concat (getenv "OCAML_TOPLEVEL_PATH")
          "/../../share/emacs/site-lisp") load-path)
;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)

;; -- Merlin setup --------------------------------------
(setq merlin-command (concat (getenv "OCAML_TOPLEVEL_PATH") "/../../bin/ocamlmerlin"))
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

;; Latex and HOL
(add-to-list 'auto-mode-alist '("\\.htex\\'" . latex-mode))

;; Spell checker for text mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; No flycheck for asciidoc files
(dolist (hook '(change-log-mode-hook log-edit-mode-hook adoc-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Asciidoc
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
(setq flycheck-global-modes '(not adoc-mode))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (adwaita)))
 '(custom-safe-themes
   (quote
	("3cd4f09a44fe31e6dd65af9eb1f10dc00d5c2f1db31a427713a1784d7db7fdfc" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" default)))
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
	(("#3C3D37" . 0)
	 ("#679A01" . 20)
	 ("#4BBEAE" . 30)
	 ("#1DB4D0" . 50)
	 ("#9A8F21" . 60)
	 ("#A75B00" . 70)
	 ("#F309DF" . 85)
	 ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
	(nyx-theme adoc-mode evil-leader proof-general smartparens auctex atom-dark-theme evil-surround flycheck auto-complete evil atom-one-dark-theme telephone-line markdown-mode sml-mode pc-bufsw monokai-theme haskell-mode dts-mode d-mode 0blayout)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(proof-three-window-mode-policy (quote hybrid))
 '(show-paren-mode t)
 '(tetris-x-colors
   [[229 192 123]
	[97 175 239]
	[209 154 102]
	[224 108 117]
	[152 195 121]
	[198 120 221]
	[86 182 194]])
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#F92672")
	 (40 . "#CF4F1F")
	 (60 . "#C26C0F")
	 (80 . "#E6DB74")
	 (100 . "#AB8C00")
	 (120 . "#A18F00")
	 (140 . "#989200")
	 (160 . "#8E9500")
	 (180 . "#A6E22E")
	 (200 . "#729A1E")
	 (220 . "#609C3C")
	 (240 . "#4E9D5B")
	 (260 . "#3C9F79")
	 (280 . "#A1EFE4")
	 (300 . "#299BA6")
	 (320 . "#2896B5")
	 (340 . "#2790C3")
	 (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
	(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 113 :width normal))))
 '(hol-bound-variable ((t (:foreground "#009900" :weight bold))))
 '(hol-free-variable ((t (:foreground "goldenrod" :weight bold))))
 '(hol-type ((t (:foreground "cyan3" :slant italic :weight bold))))
 '(hol-type-variable ((t (:foreground "purple" :slant italic :weight bold))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray30" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Monospace"))))
 '(markup-meta-hide-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-0-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-1-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-2-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-3-face ((t (:inherit markup-gen-face :weight normal :height 1.0))))
 '(show-paren-match ((t (:background "dark gray")))))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; Display time in my status bar
(display-time-mode 1)
