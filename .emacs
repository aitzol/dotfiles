(require 'tramp-cmds)
(require 'package)
(package-initialize)
(add-to-list 'package-archives
;	     '("marmalade" . "http://marmalade.ferrier.me.uk/packages/") t)
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
	     
(setq twittering-use-master-password t)


(add-to-list 'load-path "~/.emacs.d/lisp")
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-m" 'magit-status)
(exec-path-from-shell-initialize)
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; PYTHON-MODE

(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
(autoload 'doctest-mode "doctest-mode" "doctest-mode" t)


(add-to-list 'auto-mode-alist '("\\.cpy$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.vpy$" . python-mode))

(setq python-mode-hook
	'(lambda () (progn
		      (set-variable 'py-indent-offset 4)
		      (set-variable 'py-smart-indentation nil)
		      (set-variable 'indent-tabs-mode nil)
		      (local-set-key  [(meta p) (meta p)] 'break)
                      (local-set-key  [(meta p) (meta l)] 'log) )))



(add-to-list 'auto-mode-alist '("\\.cpt$" . rnc-mode))
(add-to-list 'auto-mode-alist '("\\.pt" . rnc-mode))
(add-to-list 'auto-mode-alist '("\\.zpt$" . rnc-mode))
(add-to-list 'auto-mode-alist '("\\.dtml$" . rnc-mode))
(add-to-list 'auto-mode-alist '("\\.zcml$" . rnc-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))



;; fancy parens and such:
(set-cursor-color "white")
(setq blink-cursor nil)
(show-paren-mode t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(py-pychecker-command "pychecker.sh")
 '(py-pychecker-command-args (quote ("")))
 '(tool-bar-mode nil))

 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;Laguntzak

(fset 'break
      "import pdb;pdb.set_trace()\n")
(fset 'log
      "\nfrom logging import getLogger\nlog = getLogger('')\n\n")


  (define-skeleton django-views
    "Django views plantila\n"
    "from django.http import HttpResponseRedirect\n"
    "from django.shortcuts import render_to_response\n"
    "from django.core.urlresolvers import reverse\n"
    "from django.template import RequestContext\n"
    "\n"
    )

(add-hook 'nxml-mode-hook (lambda()
 	   (setq indent-tabs-mode nil)
 	 ))

(setq nxml-slash-auto-complete-flag t)
(setq nxml-sexp-element-flag t)

;; code checking via flymake
;; set code checker here from "epylint", "pyflakes"
(setq pycodechecker "pyflakes")
(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
   (let ((help (get-char-property (point) 'help-echo)))
    (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)

(defun igor ()
  "Run the current buffer through mr.igor."
  (interactive)
  (let ((igor-exe (or (executable-find "igor")
                      (error "No command 'igor' found")))
        (tempfile (make-temp-file "igor"))
        (buffer (current-buffer))
        (lines-before (count-lines 1 (buffer-size))))
    (with-temp-file tempfile
      (insert-buffer-substring buffer))
    (with-temp-buffer
      (shell-command (concat igor-exe " --print " tempfile) t)
      (if (zerop (compare-buffer-substrings
                  (current-buffer) 1 (buffer-size)
                  buffer 1 (buffer-size buffer)))
          (message "igor: no new imports")
        (copy-to-buffer buffer 1 (buffer-size))
        (let ((lines-after (count-lines 1 (buffer-size))))
          (message "igor: added %d imports" (- lines-after lines-before))))
      (delete-file tempfile))))

(add-hook 'python-mode-hook '(lambda () (local-set-key (kbd "C-c C-i") 'igor)))

;; Show git branch on status line
(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  (setq ad-return-value
    (concat ad-return-value
            (let ((plus-minus (vc-git--run-command-string
                               file "diff" "--numstat" "--")))
              (and plus-minus
                   (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus)
                   (format " +%s-%s" (match-string 1 plus-minus) (match-string 2 plus-minus)))))))
(package-initialize)
(elpy-enable)
