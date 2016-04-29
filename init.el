;; Setup
(defun ensure-package (package-name)
  (unless (package-installed-p package-name)
    (package-refresh-contents)
    (package-install package-name)))

(defun ensure-packages (packages)
  (when packages
    (ensure-package (car packages))
    (ensure-packages (cdr packages))))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(ensure-packages '(ack
                   ac-js2
                   auto-complete
                   cider
                   clojure-mode
                   company
                   evil
                   evil-paredit
                   ido-ubiquitous
                   js3-mode
                   paredit
                   projectile
                   skewer-mode
                   smartparens
                   smooth-scrolling
                   tern
                   tern-auto-complete))

(require 'auto-complete)
(require 'cider)
(require 'evil)
(require 'evil-paredit)
(require 'ido-ubiquitous)
(require 'projectile)
(require 'smartparens-config)
(require 'tern-auto-complete)
(require 'whitespace)

;; Custom functions
(defun bind-evil (keyseq symbol &optional state-map)
  (define-key (or state-map evil-normal-state-map) (kbd keyseq) symbol))

(defun change-parenthesis ()
  (interactive)
  (save-excursion
    (let* ((c (char-to-string (following-char)))
           (open-paren? (or (string= "(" c) (string= "[" c) (string= "{" c)))
           (close-paren? (or (string= ")" c) (string= "]" c) (string= "}" c)))
           (change-paren-left (lambda (f)
                                (funcall f)
                                (forward-char)
                                (paredit-splice-sexp)))
           (wrap-fn (pcase last-command-event
                      (40 'paredit-wrap-round) ; "("
                      (91 'paredit-wrap-square) ; "["
                      (123 'paredit-wrap-curly)))) ; "{"
      (when wrap-fn
        (cond (open-paren? (funcall change-paren-left wrap-fn))
              (close-paren? (paredit-backward-up)
                            (funcall change-paren-left wrap-fn)))))))

(defvar loaded-theme)
(defun toggle-theme (theme)
  (set-frame-parameter nil 'background-mode theme)
  (set-terminal-parameter nil 'background-mode theme)
  (enable-theme 'solarized)
  (setq loaded-theme theme))

;; Hooks
(defun cider-hook ()
  (company-mode 1))

(defun clojure-hook ()
  (bind-evil "M-." 'cider-find-var)
  (bind-evil ",cc" 'cider-connect)
  (bind-evil ",d" 'cider-doc)

  (define-clojure-indent
    (fact 'defun)
    (fnk 'defun)
    (some-> 1)
    (some->> 1)
    (s/defrecord 'defun)))

(defun js-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (js2-highlight-unused-variables-mode 1)
  (tern-mode t)
  (tern-ac-setup)
  (bind-evil "M-." 'tern-find-definition)
  (bind-evil "M-," 'tern-pop-find-definition))

(defun lisp-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (paredit-mode 1)
  (evil-paredit-mode 1)

  (bind-evil ",>" 'paredit-forward-slurp-sexp)
  (bind-evil ",<" 'paredit-forward-barf-sexp)
  (bind-evil ",r" 'paredit-splice-sexp-killing-backward)
  (bind-evil "M-[" 'paredit-wrap-square)
  (bind-evil "M-{" 'paredit-wrap-curly)
  (dolist (parenthesis '("(" "[" "{"))
    (bind-evil (concat ",c" parenthesis) 'change-parenthesis)))

(add-hook 'cider-mode-hook 'cider-hook)
(add-hook 'clojure-mode-hook 'lisp-hook)
(add-hook 'clojure-mode-hook 'clojure-hook)
(add-hook 'emacs-lisp-mode-hook 'lisp-hook)
(add-hook 'js3-mode-hook 'js2-minor-mode)
(add-hook 'js3-mode-hook 'ac-js2-mode)
(add-hook 'js3-mode-hook 'js-hook)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Autocomplete
(global-auto-complete-mode t)

;; Cider
(setq-default
 cider-auto-select-error-buffer t
 cider-popup-stacktraces t
 cider-repl-history-file "/tmp/cider-repl-history"
 cider-repl-history-size 1000
 cider-repl-popup-stacktraces t
 cider-repl-pop-to-buffer-on-connect nil
 cider-repl-wrap-history t)

;; Evil
(evil-mode t)
(bind-evil "\\b" 'projectile-switch-to-buffer)
(bind-evil "\\p" 'projectile-find-file)
(bind-evil "\\s" (lambda ()
                   (interactive)
                   (toggle-theme (if (eq 'dark loaded-theme) 'light 'dark))))
(bind-evil "-" 'evil-window-next)
(bind-evil "C-j" (lambda ()
                   (interactive)
                   (evil-scroll-down nil)))
(bind-evil "C-k" (lambda ()
                   (interactive)
                   (evil-scroll-up nil)))
(bind-evil "C-z" 'suspend-frame)
(bind-evil "gc" 'comment-or-uncomment-region evil-visual-state-map)
(bind-evil "gcc" (lambda ()
                   (interactive)
                   (comment-or-uncomment-region (line-beginning-position)
                                                (line-end-position))))

;; Ido
(setq-default
 ido-enable-flex-matching t
 ido-everywhere t)

(ido-ubiquitous-mode 1)

;; Js2, js3
(setq-default
 js2-highlight-level 3
 js3-indent-dots t)

;; Projectile
(projectile-global-mode t)

;; Smartparens
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "M-("))

(custom-set-variables
 '(sp-base-key-bindings 'paredit)
 '(sp-autoskip-closing-pair 'always))

;; Solarized
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized t)
(toggle-theme 'light)

;; Smooth scrolling
(setq-default
 scroll-margin 5
 scroll-conservatively 9999
 scroll-step 1)

;; Whitespace
(setq-default
 whitespace-action '(auto-cleanup)
 whitespace-style '(empty face tabs indentation lines-tail space-after-tab
                    space-before-tab trailing)
 whitespace-line-column 81)

(global-whitespace-mode t)

;; Global config
(setq-default
 ack-default-directory-function '(lambda (&rest args) (projectile-project-root))
 make-backup-files nil
 column-number-mode t
 delete-old-versions t
 indent-tabs-mode nil
 line-number-mode t
 tab-width 2)

(global-linum-mode t)
(global-set-key (kbd "RET") 'newline-and-indent)
(menu-bar-mode -1)
