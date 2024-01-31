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
  (org-roam-directory (file-truename "/Users/daniel.munoz/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Documents/ORG-ROAM"))
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

;;Guess-language
(require 'guess-language)
(setq guess-language-languages '(en es))
(setq guess-language-min-paragrah-length 35)
(add-hook 'text-mode-hook (lambda () (guess-language-mode 1)))

;;Latex
(setq org-latex-src-block-backend 'listings)
