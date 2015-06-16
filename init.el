(defalias 'vi 'evil-mode)

(defun ensure-package (package-name)
  (unless (package-installed-p package-name)
    (package-refresh-contents)
    (package-install package-name)))

(defun ensure-packages (packages)
  (when packages
    (ensure-package (car packages))
    (ensure-packages (cdr packages))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(ensure-packages '(auto-complete
                    cider
                    clojure-mode
                    company
                    evil
                    evil-paredit
                    ido-ubiquitous
                    paredit
                    projectile
                    smartparens))

(require 'auto-complete)
(require 'cider)
(require 'evil)
(require 'evil-paredit)
(require 'ido-ubiquitous)
(require 'projectile)
(require 'smartparens-config)

(defun bind-evil (keyseq symbol &optional state-map)
  (define-key (or state-map evil-normal-state-map) (kbd keyseq) symbol))

(defun change-parenthesis-at-open (wrap-fn)
  (funcall wrap-fn)
  (forward-char)
  (paredit-splice-sexp))

(defun change-parenthesis-at-close (wrap-fn)
  (paredit-backward-up)
  (change-parenthesis-at-open wrap-fn))

(defun open-parenthesisp (c)
  (or (string= "(" c) (string= "[" c) (string= "{" c)))

(defun close-parenthesisp (c)
  (or (string= ")" c) (string= "]" c) (string= "}" c)))

(defun change-parenthesis ()
  (interactive)
  (save-excursion
    (let ((c (char-to-string (following-char)))
          (wrap-fn (cond ((= 40 last-command-event) 'paredit-wrap-round) ; "("
                         ((= 91 last-command-event) 'paredit-wrap-square) ; "["
                         ((= 123 last-command-event) 'paredit-wrap-curly)))) ; "{"
      (when wrap-fn
        (cond ((open-parenthesisp c) (change-parenthesis-at-open wrap-fn))
              ((close-parenthesisp c) (change-parenthesis-at-close wrap-fn)))))))

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

(defun clojure-hook ()
  (bind-evil "M-." 'cider-find-var)
  (bind-evil ",cc" 'cider-connect)
  (bind-evil ",d" 'cider-doc)

  (define-clojure-indent
    (fact 'defun)
    (fnk 'defun)
    (go-try 'defun)))

(defun cider-hook ()
  (cider-turn-on-eldoc-mode)
  (company-mode 1))

(setq cider-auto-select-error-buffer t
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
(bind-evil "C-k" (lambda ()
                   (interactive)
                   (evil-scroll-up nil)))
(bind-evil "C-j" (lambda ()
                   (interactive)
                   (evil-scroll-down nil)))
(bind-evil "C-z" 'suspend-frame)
(bind-evil "-" 'evil-window-next)
(bind-evil "gc" 'comment-or-uncomment-region evil-visual-state-map)
(bind-evil "gcc" (lambda ()
                   (interactive)
                   (comment-or-uncomment-region (line-beginning-position)
                                                (line-end-position))))


(global-set-key (kbd "RET") 'newline-and-indent)

;; Smartparens
(sp-with-modes sp--lisp-modes
               (sp-local-pair "(" nil :bind "M-("))

;; Autocomplete
(global-auto-complete-mode t)

;; Solarized
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized t)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)


(add-hook 'emacs-lisp-mode-hook 'lisp-hook)
(add-hook 'clojure-mode-hook 'lisp-hook)
(add-hook 'clojure-mode-hook 'clojure-hook)
(add-hook 'cider-mode-hook 'cider-hook)

(custom-set-variables
  '(sp-base-key-bindings 'paredit)
  '(sp-autoskip-closing-pair 'always))

(setq
  ack-default-directory-function '(lambda (&rest args) (projectile-project-root))
  ido-enable-flex-matching t
  ido-everywhere t
  tab-width 2)

(global-linum-mode t)
(ido-ubiquitous-mode 1)
