;; We do that in first so that the them can load and fast
(package-initialize)

;; Setting the theme
(load-theme 'monokai t)

;; Disable emacs startup screens
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; Setting the packages stuff
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)

;; Telling emacs to stop spreading files with ~ everywhere
(setq make-backup-files nil)

;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it on startup.
(setq-default message-log-max nil)
(when (get-buffer "*Messages*") (kill-buffer "*Messages*"))

;; Load stuff
(require 'ido)
(ido-mode t)

;; Parenthesis
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

;; Helm initialisation
(require 'helm-config)
  
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
  "f" 'helm-find-files
  "<SPC>" 'save-buffer
  "b" 'helm-buffers-list
  "<right>" 'next-buffer
  "<left>" 'previous-buffer
  "q" 'save-buffers-kill-terminal
  "x" 'helm-M-x)

;; Leader bindings for coq mode
(evil-leader/set-key-for-mode 'coq-mode
  "<down>" 'proof-assert-next-command-interactive
  "<up>" 'proof-undo-last-successful-command
  "<right>" 'proof-goto-point
  "<home>" 'proof-retract-buffer
  "<end>" 'proof-process-buffer)


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
(autoload 'utop-minor-mode "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

;; -- Merlin setup --------------------------------------
(setq merlin-command (concat (getenv "OCAML_TOPLEVEL_PATH") "/../../bin/ocamlmerlin"))
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

;; ocp-indent setup
(add-to-list 'load-path "/home/qladevez/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

;; Auto-completion set-up
(require 'auto-complete)
(ac-config-default)
(setq merlin-ac-setup t)
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

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
 '(package-selected-packages
   (quote
	(ac-helm helm use-package telephone-line spacemacs-theme proof-general nyx-theme monokai-theme helm-core geiser evil-surround evil-leader auto-complete atom-one-dark-theme adoc-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markup-meta-hide-face ((t (:inherit markup-meta-face :foreground "#66D9EF" :height 1.0))))
 '(markup-title-0-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-1-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-2-face ((t (:inherit markup-gen-face :height 1.0))))
 '(markup-title-3-face ((t (:inherit markup-gen-face :weight normal :height 1.0)))))
