;;; A quick way to run JUnit tests:
;;; `C-x t u` runs all the test cases in a unit.
;;; `C-x t c` runs a single test case.
;;;
;;; Simply add the snippet at the end of `init.el` or `.emacs`.
;;; NOTE: It assumes you are using projectile to manage the project.
;;;
;;; By Bahman Movaqar

;; prompts for a single test case in the current class and runs it
;; Function to run a single test case
;;;; test-runner.el --- A quick way to run JUnit tests.

;; This configuration helps imenu properly find test methods in Java files.
(eval-after-load 'java-mode
  '(progn
     (setq imenu-generic-expression
           '(("Tests" "^\\s-*@Test\\s-.*void\\s+\\([a-zA-Z0-9_]+\\)" 1)
             ("Methods" "^\\s-*\\(public\\|private\\|protected\\).*\\s[\\w<>]+\\s+\\([a-zA-Z0-9_]+\\)" 2)))))

(defun my-get-imenu-test-names ()
  "Parse the complex imenu list from lsp-mode to get clean test names."
  (let ((imenu-data (imenu--make-index-alist))
        (test-names ()))
    (dolist (item imenu-data)
      ;; This check is now more robust. It ensures we only process items
      ;; that are deeply nested lists (like test methods) and safely skips
      ;; simple items like ("*Rescan*" . -99) that caused the error.
      (when (and (consp item)
                 (listp (cdr item))
                 (consp (cadr item)))
        (let* ((method-def (cadr item))
               (method-info (car method-def)))
          (push (substring-no-properties method-info) test-names))))
    (nreverse test-names)))

(defun run-test-case ()
  "Prompts for a single test case and runs it using the compile command."
  (interactive)
  (let ((root (projectile-project-root)))
    (unless root
      (error "You are not in a Projectile project. Cannot find project root."))

    (let* ((class-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           ;; 1. Use our new helper function to get a clean list of names.
           (test-case-names (my-get-imenu-test-names))
           (selected-item (completing-read "Test case: " test-case-names nil t))
           ;; 2. Robustly extract just the method name from the selection.
           (test-case (when (string-match "^\\([a-zA-Z0-9_]+\\)" selected-item)
                        (match-string 1 selected-item)))
           (mvn-cmd (concat "mvn -Dtest="
                            class-name
                            "#"
                            test-case
                            " test")))
      (when test-case
        (let ((default-directory root))
          (compile mvn-cmd))))))


;; Key bindings should be at the top level of the file.
(global-set-key (kbd "C-x t c") 'run-test-case)
