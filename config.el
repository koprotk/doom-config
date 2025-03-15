;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Daniel Muñoz"
      user-mail-address "daniel.munoz@modemed.com")

(setq default-directory (concat(getenv "HOME") "/"))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;;(setq doom-theme 'doom-one)
;;(setq doom-theme 'doom-one-light)

;; Change the Theme by the sunset sunrise in my timezone
(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude -33.4489)
  (setq calendar-longitude -70.6693)
  (setq circadian-themes '((:sunrise . doom-one-light)
                           (:sunset  . doom-one)))
  (circadian-setup))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory '("~/org"))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;Autosave
(setq auto-save-default t
      make-backup-files t)

;;Org mode config
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (directory-files-recursively "~/org" "\\.org$" :follow-symlinks t)) ;;Reviar la forma en que org y org-agenda usen los mismo archivos.
(setq calendar-week-start-day 1) ;;Inicio de la semana el Lunes b
(setq org-agenda-include-diary t) ;;Incluír entradas en  org-agenda
(setq org-agenda-start-with-log-mode t) ;;Mostrar los TO DO cerrados

;;Custom To DO
;; (setq org-todo-keywords
;;       '((sequence "TO BUILD" "BUILDED" "TESTING" "COMMENTING" "|" "DONE")
;;         (sequence "[ ]" "[-]" "|" "[x]")
;;         (sequence "TODO" "|" "DONE")
;;         )
;;       )

;;Holidays in orgmode
'(holiday-bahai-holidays nil) ;;don't show bahai holiday in calendar
'(holiday-hebrew-holidays nil)
'(holiday-islamic-holidays nil)
(setq holiday-other-holidays
      '(
        (holiday-fixed 7 16 "Día de la Virgen del Carmen") ;;holiday-fixedd month day "name of the holiday"
        (holiday-fixed 8 15 "Asunción de la Viergen")
        (holiday-fixed 9 18 "Independencia Nacional")
        (holiday-fixed 9 19 "Día de las glorias del ejercito")
        (holiday-fixed 10 9 "Encuentro entre dos mundos")
        (holiday-fixed 10 27 "Día de las Iglesias Evangélicas y Protestantes")
        (holiday-fixed 11 1 "Día de todos los Santos")
        (holiday-fixed 12 8 "Inmaculada Concepción")
        (holiday-fixed 12 17 "Plebiscito Nacional")
        (holiday-fixed 12 25 "Navidad")
        )
      )

;;Org mode ROAM config

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org/org-roam"))
  :config
  (org-roam-db-autosync-enable)
  )
;;:bind (("C-c n i" . org-roam-find)
;;       ("C-c n r" . org-roam-random)
;;       (:map org-mode-map
;;             (("C-c n i" . org-roam-node-insert)
;;              ("C-c n o" . org-id-get-create)
;;              ("C-c n t" . org-roam-tag-add)
;;              ("C-c n a" . org-roam-alias-add)
;;              ("C-c m l" . org-roam-buffer-toggle)))))

;;Org Babel
(setq ob-async-no-async-languages-alist '("python" "jupyter-python"))
(setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
(setq +python-jupyter-repl-args '("--simple-prompt"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) ;; Other languages
   (shell . t)
   ;; Python & Jupyter
   (python . t)
   (jupyter . t)))

;;(org-babel-jupyter-override-src-block "python")
(setenv "PYDEVD_DISABLE_FILE_VALIDATION" "1")

;;Delete style marks
(setq org-hide-emphasis-markers t)

;;Org-journal
(setq org-journal-dir "~/org/journal")
(setq org-journal-date-format "%A, %d %B %Y")
(setq org-journal-file-type 'monthly)
(setq org-journal-enable-agenda-integration t)
(require 'org-journal)

;;Latex
(add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
(add-hook 'org-mode-hook #'turn-on-org-cdlatex)
(setq org-latex-src-block-backend 'listings)

(add-to-list 'load-path "/usr/local/bin")
(add-to-list 'load-path "~/.doom.d/lsp-latex")
(add-to-list 'load-path "~/.cargo/bin")
(require 'lsp-latex)

(add-hook 'latex-mode-hook (lambda() (add-to-list 'tex-compile-commands '("latexmk -pv -pdf -bibtex %r"))))
;; "texlab" executable must be located at a directory contained in `exec-path'.
;; If you want to put "texlab" somewhere else,
;; you can specify the path to "texlab" as follows:
;; (setq lsp-latex-texlab-executable "/usr/local/bin/texlab")

(with-eval-after-load "tex-mode"
  (add-hook 'tex-mode-hook 'lsp)
  (add-hook 'latex-mode-hook 'lsp)
  ) ;;with AUCTeX LaTeX mode

;; For YaTeX
(with-eval-after-load "yatex"
  (add-hook 'yatex-mode-hook 'lsp))

;; For bibtex
(with-eval-after-load "bibtex"
  (add-hook 'bibtex-mode-hook 'lsp))

;;Projectile
(after! projectile
  (setq projectile-project-search-path '("~/Projects/"))
  (setq projectile-enable-caching t))

;;Java TestNg custom command:
(defun get-testng-classname ()
  "Get the fully qualified TestNG class name from the current buffer."
  (let* ((default-directory (project-root (project-current t))) ;; Ensure execution from project root
         (file-path (file-relative-name (buffer-file-name) (concat default-directory "src/"))) ;; Get relative path
         (test-class (replace-regexp-in-string "/" "." (file-name-sans-extension file-path)))) ;; Convert to FQCN
    test-class)) ;; Return class name

(defun run-testng ()
  "Run a TestNG test from the current buffer.
Prompts the user for a method name. If left empty, runs the whole test class.
Otherwise, runs the specified method."
  (interactive)
  ;; Ensure projectile is open before

  (let* ((classpath "./build/libs/dependencies/*:./build/classes/java/main:./build/resources/main")
         (config-dir "./build/classes/java/main/com/modmed/bugspray/config")
         (testng-classpath "./test/com/modmed/bugspray/core/suites")
         (test-class (get-testng-classname)) ;; Use reusable function
         (method-name (read-string "Enter method name (leave empty to run entire class): "))
         (testng-args (if (string-empty-p method-name)
                          (format "-testclass %s" test-class)
                        (format "-methods %s.%s" test-class method-name)))
         (default-directory (projectile-project-root))

         (command (format "./gradlew build && java -Xms512m -Xmx1024m -cp \"%s\" -Dconfig.dir=%s -Dtestng.test.classpath=%s org.testng.TestNG %s"
                          classpath config-dir testng-classpath testng-args)))

    ;; Debugging messages
    (message "This is the default directory: %s" default-directory)
    (message "This is the get-testng-classname: %s" test-class)
    ;; Run the command
    (projectile-run-async-shell-command-in-root command)))


(defun copy-testng-classname ()
  "Copy the fully qualified TestNG class name to the clipboard."
  (interactive)
  (let ((test-class (get-testng-classname)))
    (kill-new test-class)
    (message "Copied to clipboard: %s" test-class)))

(defun run-testng-debug ()
  "Compile the project, then run TestNG test in JDB inside a vterm shell.
If a method is provided, use -method instead of -testclass."
  (interactive)
  (let* ((sourcepath "./build/classes/java/main:./build/resources/main") ;; Ensure project root
         (classpath "./build/libs/dependencies/*:./build/classes/java/main:./build/resources/main")
         (config-dir "./build/classes/java/main/com/modmed/bugspray/config")
         (testng-classpath "./test/com/modmed/bugspray/core/suites")
         (test-class (or (get-testng-classname) "")) ;; Get test class
         (method-name (read-string "Enter method name (leave empty to run whole class): " nil nil "")) ;; Ask user for method
         (testng-args (if (string-empty-p method-name)
                          (format "-testclass %s" test-class)
                        (format "-methods %s.%s" test-class method-name)))
         (default-directory (projectile-project-root))
         (command (format "./gradlew build && jdb -Xms512m -Xmx1024m -sourcepath \"%s\" -classpath \"%s\" -Dtestng.test.classpath=%s -Dconfig.dir=%s org.testng.TestNG %s"
                          sourcepath classpath testng-classpath config-dir testng-args)))
    (compile command)))

(map! :leader
      (:prefix ("m" . "TestNG")
       :desc "Run TestNG test" "r" #'run-testng
       :desc "Debug TestNG test" "d" #'run-testng-debug
       :desc "Copy TestNG class" "c" #'copy-testng-classname))
