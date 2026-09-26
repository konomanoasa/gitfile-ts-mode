;;; git-ts-mode-test.el --- Tests for git-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 konomanoasa
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'ert)
(require 'imenu)
(require 'loaddefs-gen)
(require 'newcomment)
(require 'git-ts-mode)

(dolist (language '(gitignore gitattributes gitconfig))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defun git-ts-mode-test--position (fragment &optional line)
  (save-excursion
    (goto-char (point-min))
    (when line
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (equal line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
              (setq found t)
            (forward-line 1)))
        (unless found (ert-fail (format "Missing fixture line: %S" line)))))
    (unless (search-forward fragment (and line (line-end-position)) t)
      (ert-fail (format "Missing fixture fragment: %S" fragment)))
    (- (point) (length fragment))))

(defun git-ts-mode-test--face (fragment &optional offset line)
  (get-text-property (+ (git-ts-mode-test--position fragment line)
                        (or offset 0)) 'face))

(defun git-ts-mode-test--syntax-class (fragment &optional offset line)
  (syntax-propertize (point-max))
  (syntax-class (syntax-after (+ (git-ts-mode-test--position fragment line)
                                 (or offset 0)))))

(defun git-ts-mode-test--buffer-state ()
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun git-ts-mode-test--should-match-fresh-buffer (level)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (git-ts-mode-test--buffer-state))
        (file buffer-file-name))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (git-ts-mode))
      (should (equal state (git-ts-mode-test--buffer-state))))))

;;;; Grammar

(ert-deftest git-ts-mode-respects-grammar-sources ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) received)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language)
                  (setq received (assq language treesit-language-source-alist))
                  t))
          (dolist (source git-ts-mode--grammar-sources)
            (let* ((language (car source))
                   (custom (list language "/local/grammar" :revision "custom")))
              (dolist (configured (list nil (list custom)))
                (let ((treesit-language-source-alist configured))
                  (should (git-ts-mode--ensure-grammar language))
                  (should (equal received (if configured custom source)))
                  (should (eq treesit-language-source-alist configured)))))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest git-ts-mode-reports-unavailable-selected-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) requested)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language) (push language requested) nil))
          (with-temp-buffer
            (setq buffer-file-name "/tmp/.gitconfig")
            (should-error (git-ts-mode) :type 'user-error)
            (should (equal requested '(gitconfig)))
            (should-not (treesit-parser-list))))
      (fset 'treesit-ensure-installed ensure))))

;;;; Mode Selection

(ert-deftest git-ts-mode-selects-only-the-file-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) requested)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language) (push language requested) t))
          (pcase-dolist (`(,file . ,language)
                         '(("/tmp/.gitignore" . gitignore)
                           ("project.gitignore" . gitignore)
                           ("/tmp/.gitattributes" . gitattributes)
                           ("work.gitattributes" . gitattributes)
                           ("/tmp/.gitconfig" . gitconfig)
                           ("work.gitconfig" . gitconfig)))
            (with-temp-buffer
              (setq buffer-file-name file requested nil)
              (insert "# note\n")
              (set-auto-mode)
              (font-lock-ensure)
              (should (eq major-mode 'git-ts-mode))
              (should (equal requested (list language)))
              (should (eq (treesit-parser-language treesit-primary-parser) language))
              (should (equal (treesit-parser-list) (list treesit-primary-parser)))
              (git-ts-mode)
              (should (equal (treesit-parser-list) (list treesit-primary-parser))))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest git-ts-mode-rejects-unknown-file-names-before-loading-grammars ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (_language) (ert-fail "Unexpected grammar installation")))
          (dolist (file '(nil "config" "attributes" "exclude" ".git/config"
                              ".GITIGNORE" ".gitConfig" "a.gitattributes.bak"
                              "/tmp/.gitignore/config"))
            (with-temp-buffer
              (setq buffer-file-name file)
              (should-error (git-ts-mode) :type 'user-error)
              (should-not (treesit-parser-list)))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest git-ts-mode-reselects-language-on-restart ()
  (with-temp-buffer
    (setq buffer-file-name ".gitignore")
    (insert "[core]\nfilemode=true\n")
    (git-ts-mode)
    (setq buffer-file-name ".gitconfig")
    (should (eq (treesit-parser-language treesit-primary-parser) 'gitignore))
    (git-ts-mode)
    (should (eq (treesit-parser-language treesit-primary-parser) 'gitconfig))
    (should (equal (treesit-parser-list) (list treesit-primary-parser)))
    (font-lock-ensure)
    (should (eq (git-ts-mode-test--face "core") 'font-lock-type-face))))

(ert-deftest git-ts-mode-generates-autoloads ()
  (let ((output (make-temp-file "git-ts-mode-loaddefs-"))
        (directory (file-name-directory (locate-library "git-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'git-ts-mode" "(add-to-list 'auto-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))))
      (delete-file output))))

;;;; Syntax

(ert-deftest git-ts-mode-keeps-quotes-and-backslashes-as-punctuation ()
  (dolist (case '((".gitignore" "'`\\\"\n")
                  (".gitattributes" "\"'`\\\"\" text\n")
                  (".gitconfig" "[core]\nvalue=\"'`\\\"\"\n")))
    (with-temp-buffer
      (setq buffer-file-name (car case))
      (insert (cadr case))
      (git-ts-mode)
      (dolist (fragment '("'" "`" "\\" "\""))
        (should (= (git-ts-mode-test--syntax-class fragment) 1))))))

(ert-deftest git-ts-mode-classifies-glob-delimiters ()
  (dolist (file '(".gitignore" ".gitattributes"))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert "[a-z[:digit:]][[.ch.]][[=e=]]\n\\[literal()]\n")
      (git-ts-mode)
      (dolist (entry '(("[a" . 4) ("[:" . 4) (":digit" . 1)
                       (":]" . 1) ("]][[" . 5) ("[." . 1) (".ch" . 1)
                       ("[=" . 1) ("=e" . 1) ("\\[" . 1)
                       ("()]" . 1) (")]" . 1)))
        (should (= (git-ts-mode-test--syntax-class (car entry)) (cdr entry))))
      (should (= (git-ts-mode-test--syntax-class "\\[" 1) 1))
      (should (= (scan-sexps 1 1) (git-ts-mode-test--position "[[.ch.]]"))))))

(ert-deftest git-ts-mode-keeps-encoded-brackets-and-macro-markers-as-punctuation ()
  (let ((treesit-font-lock-level 4))
    (with-temp-buffer
      (setq buffer-file-name ".gitattributes")
      (insert "\"\\133\\133:digit:\\135\\135\" text\n[attr]binary -diff\n")
      (git-ts-mode)
      (font-lock-ensure)
      (syntax-propertize (point-max))
      (dolist (fragment '("\\133" "\\135" "[attr]" "]binary"))
        (should (= (git-ts-mode-test--syntax-class fragment) 1)))
      (should (eq (git-ts-mode-test--face "\\133") 'font-lock-bracket-face))
      (should (eq (git-ts-mode-test--face "\\135") 'font-lock-bracket-face))
      (should (eq (git-ts-mode-test--face "digit") 'font-lock-type-face))
      (should (eq (git-ts-mode-test--face "[attr]") 'font-lock-keyword-face)))))

(ert-deftest git-ts-mode-classifies-config-delimiters ()
  (with-temp-buffer
    (setq buffer-file-name ".gitconfig")
    (insert "[core \"[sub]\"]\nvalue=\"([{}])\"\n[unfinished")
    (git-ts-mode)
    (dolist (entry '(("[core" . 4) ("]\nvalue" . 5) ("[sub" . 1)
                     ("\"([" . 1) ("([{" . 1) ("[unfinished" . 4)))
      (should (= (git-ts-mode-test--syntax-class (car entry)) (cdr entry))))))

(ert-deftest git-ts-mode-classifies-only-cst-comments ()
  (pcase-dolist (`(,file ,source ,comments ,literals)
                 '((".gitignore" "# note\n # literal\n\\# escaped\n"
                    ("note") ("literal" "escaped"))
                   (".gitattributes" "  # note\n*.c attr=#literal\n\"#quoted\" attr\n"
                    ("note") ("literal" "quoted"))
                   (".gitconfig" "# note\n[core]\na=\"#quoted;value\" ; inline\nb=next\n"
                    ("note" "inline") ("quoted" "value" "next"))))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (git-ts-mode)
      (syntax-propertize (point-max))
      (dolist (fragment comments)
        (should (nth 4 (syntax-ppss (git-ts-mode-test--position fragment)))))
      (dolist (fragment literals)
        (should-not (nth 4 (syntax-ppss (git-ts-mode-test--position fragment))))))))

(ert-deftest git-ts-mode-preserves-comment-text-through-carriage-returns ()
  (dolist (file '(".gitignore" ".gitattributes" ".gitconfig"))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert "# before\rafter\r\nnext\n")
      (git-ts-mode)
      (syntax-propertize (point-max))
      (should (nth 4 (syntax-ppss (git-ts-mode-test--position "after"))))
      (should-not (nth 4 (syntax-ppss (git-ts-mode-test--position "next")))))))

(ert-deftest git-ts-mode-comments-and-uncomments-without-removing-pattern-spaces ()
  (with-temp-buffer
    (setq buffer-file-name ".gitignore")
    (insert "  leading\n\t tab\nplain\n")
    (git-ts-mode)
    (comment-region (point-min) (point-max))
    (should (equal (buffer-string) "#   leading\n# \t tab\n# plain\n"))
    (uncomment-region (point-min) (point-max))
    (should (equal (buffer-string) "  leading\n\t tab\nplain\n"))))

(ert-deftest git-ts-mode-comments-attributes-and-config ()
  (pcase-dolist (`(,file ,source ,commented)
                 '((".gitattributes" "*.c text\n" "# *.c text\n")
                   (".gitconfig" "key=value\n" "# key=value\n")))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (git-ts-mode)
      (comment-region (point-min) (point-max))
      (should (equal (buffer-string) commented))
      (uncomment-region (point-min) (point-max))
      (should (equal (buffer-string) source))))
  (with-temp-buffer
    (setq buffer-file-name ".gitconfig")
    (insert "; note\n")
    (git-ts-mode)
    (uncomment-region (point-min) (point-max))
    (should (equal (buffer-string) "note\n"))))

;;;; Electric Pair

(ert-deftest git-ts-mode-pairs-delimiters-and-indents-on-return ()
  (dolist (file '(".gitignore" ".gitattributes" ".gitconfig"))
    (dolist (case '(("" ?\[ "[\n]" 0)))
      (let ((electric-pair-pairs nil)
            (electric-pair-open-newline-between-pairs t))
        (with-temp-buffer
          (setq buffer-file-name file)
          (git-ts-mode)
          (setq-local indent-tabs-mode nil)
          (electric-indent-local-mode 1)
          (electric-pair-local-mode 1)
          (insert (nth 0 case))
          (let ((last-command-event (nth 1 case)))
            (self-insert-command 1))
          (should (= (char-after) (cdr (assq (nth 1 case) electric-pair-pairs))))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-string) (nth 2 case)))
          (should (= (current-column) (nth 3 case))))))))

(ert-deftest git-ts-mode-keeps-pair-preferences-local ()
  (let ((electric-pair-pairs '((?% . ?%)))
        (electric-pair-mode nil)
        (electric-pair-open-newline-between-pairs nil))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/.gitconfig")
      (git-ts-mode)
      (should-not electric-pair-mode)
      (should (equal (car electric-pair-pairs) '(?% . ?%)))
      (electric-indent-local-mode 1)
      (electric-pair-local-mode 1)
      (insert "")
      (let ((last-command-event ?\[))
        (self-insert-command 1))
      (call-interactively (key-binding (kbd "RET")))
      (should (= (count-lines (point-min) (point-max)) 2)))
    (should (equal electric-pair-pairs '((?% . ?%))))
    (should-not electric-pair-open-newline-between-pairs)))


(ert-deftest git-ts-mode-respects-custom-pair-closings ()
  (let ((electric-pair-pairs '((?\[ . ?!)))
        (electric-pair-open-newline-between-pairs (lambda () t)))
    (dolist (file '(".gitignore" ".gitattributes" ".gitconfig"))
      (with-temp-buffer
        (setq buffer-file-name file)
        (git-ts-mode)
        (electric-pair-local-mode 1)
        (let ((last-command-event ?\[)) (self-insert-command 1))
        (should (equal (buffer-string) "[!"))
        (call-interactively (key-binding (kbd "RET")))
        (should (= (count-lines (point-min) (point-max)) 2))))))

;;;; Font Lock

(ert-deftest git-ts-mode-fontifies-globs-by-level ()
  (dolist (file '(".gitignore" ".gitattributes"))
    (dolist (level '(1 2 3 4))
      (with-temp-buffer
        (setq buffer-file-name file)
        (insert "# note\n*.c/**/?/[^a-z[:digit:]]/[[.ch.]]/[[=e=]]/\\*\n")
        (let ((treesit-font-lock-level level)) (git-ts-mode))
        (font-lock-ensure)
        (dolist (entry '(("#" 1 font-lock-comment-face)
                         ("note" 1 font-lock-comment-face)
                         (".c" 2 font-lock-string-face)
                         ("a-z" 2 font-lock-string-face)
                         ("ch" 2 font-lock-string-face)
                         ("e=" 2 font-lock-string-face)
                         ("digit" 3 font-lock-type-face)
                         ("\\*" 3 font-lock-escape-face)
                         ("*.c" 4 font-lock-constant-face)
                         ("**" 4 font-lock-constant-face)
                         ("?" 4 font-lock-constant-face)
                         ("^" 4 font-lock-negation-char-face)
                         ("-z" 4 font-lock-operator-face)
                         ("/" 4 font-lock-punctuation-face)
                         (":digit" 4 font-lock-punctuation-face)
                         (".ch" 2 font-lock-string-face)
                         ("=e" 2 font-lock-string-face)
                         ("[^" 4 font-lock-bracket-face)
                         ("[:" 4 font-lock-bracket-face)
                         ("[." 2 font-lock-string-face)
                         ("[=" 2 font-lock-string-face)
                         ("]]" 4 font-lock-bracket-face)))
          (ert-info ((format "%s level %s fragment %s" file level (car entry)))
            (should (eq (git-ts-mode-test--face (car entry))
                        (and (>= level (nth 1 entry)) (nth 2 entry))))))))))

(ert-deftest git-ts-mode-fontifies-language-specific-leaves-by-level ()
  (pcase-dolist (`(,file ,source ,entries)
                 '((".gitignore" "!keep\n"
                    (("!" 4 font-lock-negation-char-face)))
                   (".gitattributes" "[attr]binary -diff !text eol=auto\n\"a\\t\" attr\n\"\\133attr]\\146oo\" text\n"
                    (("[attr]" 2 font-lock-keyword-face)
                     ("binary" 2 font-lock-variable-name-face)
                     ("text" 2 font-lock-variable-name-face)
                     ("auto" 2 font-lock-string-face)
                     ("\"a" 2 font-lock-string-face)
                     ("\\t" 3 font-lock-escape-face)
                     ("\\133attr]" 2 font-lock-keyword-face)
                     ("\\146" 3 font-lock-escape-face)
                     ("oo\"" 2 font-lock-variable-name-face)
                     ("-diff" 4 font-lock-negation-char-face)
                     ("!text" 4 font-lock-negation-char-face)
                     ("=" 4 font-lock-operator-face)))
                   (".gitconfig" "[includeIf \"gitdir:foo\"]\npath=\"a\\t\"\\\n  true ; note\n"
                    (("includeIf" 2 font-lock-type-face)
                     ("gitdir" 2 font-lock-string-face)
                     ("path" 2 font-lock-property-name-face)
                     ("true" 2 font-lock-string-face)
                     ("\"gitdir" 2 font-lock-string-face)
                     ("\\t" 3 font-lock-escape-face)
                     ("\\\n" 3 font-lock-escape-face)
                     (";" 1 font-lock-comment-face)
                     ("note" 1 font-lock-comment-face)
                     ("=" 4 font-lock-operator-face)
                     ("[" 4 font-lock-bracket-face)
                     ("]" 4 font-lock-bracket-face)))
                   (".gitconfig" "[remote.Origin.more]\nx=y\n[remote.part \"quoted\"]\n"
                    (("remote.Origin" 2 font-lock-type-face)
                     (".Origin" 4 font-lock-punctuation-face)
                     ("Origin.more" 2 font-lock-string-face)
                     ("remote.part" 2 font-lock-type-face)
                     ("quoted" 2 font-lock-string-face)))))
    (dolist (level '(1 2 3 4))
      (with-temp-buffer
        (setq buffer-file-name file)
        (insert source)
        (let ((treesit-font-lock-level level)) (git-ts-mode))
        (font-lock-ensure)
        (dolist (entry entries)
          (ert-info ((format "%s level %s fragment %s" file level (car entry)))
            (should (eq (git-ts-mode-test--face (car entry))
                        (and (>= level (nth 1 entry)) (nth 2 entry))))))))))

(ert-deftest git-ts-mode-keeps-syntax-issues-unfontified ()
  (let ((treesit-font-lock-level 4))
    (with-temp-buffer
      (setq buffer-file-name ".gitconfig")
      (insert "[co!re]\nva@r=ok\\qtail\n")
      (git-ts-mode)
      (font-lock-ensure)
      (dolist (fragment '("!" "@" "\\q" "qtail"))
        (should-not (git-ts-mode-test--face fragment)))
      (should (eq (git-ts-mode-test--face "co") 'font-lock-type-face))
      (should (eq (git-ts-mode-test--face "va") 'font-lock-property-name-face))
      (should (eq (git-ts-mode-test--face "tail") 'font-lock-string-face)))))

;;;; Navigation

(ert-deftest git-ts-mode-navigates-ignore-patterns-without-glob-internals ()
  (with-temp-buffer
    (setq buffer-file-name ".gitignore")
    (insert "!src/[a-z]*.c  \n  keep\\ file\n")
    (git-ts-mode)
    (should (equal (mapcar #'car (cdr (assq 'gitignore treesit-thing-settings)))
                   '(sexp)))
    (goto-char (git-ts-mode-test--position "a-z"))
    (forward-sexp)
    (should (= (point) (git-ts-mode-test--position "  keep")))
    (backward-sexp)
    (should (= (point) (point-min)))
    (forward-sexp 2)
    (should (= (point) (point-max)))
    (backward-sexp 2)
    (should (= (point) (point-min)))))

(ert-deftest git-ts-mode-navigates-attribute-entries-names-and-values ()
  (with-temp-buffer
    (setq buffer-file-name ".gitattributes")
    (insert "*.c -diff eol=lf !text\n[attr]binary -diff\n")
    (git-ts-mode)
    (should (equal (mapcar #'car (cdr (assq 'gitattributes treesit-thing-settings)))
                   '(sexp defun)))
    (dolist (case '(("*.c" 0 1 "[attr]" 0)
                    ("*.c" 1 1 "*.c" 3)
                    ("-diff" 0 1 "-diff" 5)
                    ("-diff" 5 -1 "-diff" 0)
                    ("eol=lf" 0 1 "eol=lf" 6)
                    ("eol=lf" 1 1 "eol=lf" 3)
                    ("lf" 0 1 "lf" 2)
                    ("lf" 2 -1 "eol=lf" 0)
                    ("!text" 0 1 "!text" 5)
                    ("binary" 0 1 "binary" 6)))
      (ert-info ((format "%S" case))
        (goto-char (+ (git-ts-mode-test--position (nth 0 case)) (nth 1 case)))
        (forward-sexp (nth 2 case))
        (should (= (point) (+ (git-ts-mode-test--position (nth 3 case))
                              (nth 4 case))))))
    (goto-char (point-min))
    (forward-sexp 2)
    (should (= (point) (point-max)))
    (backward-sexp 2)
    (should (= (point) (point-min)))))

(ert-deftest git-ts-mode-navigates-config-entries-names-and-whole-values ()
  (with-temp-buffer
    (setq buffer-file-name ".gitconfig")
    (insert "[remote \"origin\"]\nurl=pre\"quoted\"\\\n  tail\n# note\n\n[core]\nflag\n")
    (git-ts-mode)
    (should (equal (mapcar #'car (cdr (assq 'gitconfig treesit-thing-settings)))
                   '(sexp defun)))
    (dolist (case '(("[remote" 0 1 "[core]" 0)
                    ("remote" 0 1 "remote" 6)
                    ("remote" 6 -1 "remote" 0)
                    ("\"origin\"" 0 1 "\"origin\"" 8)
                    ("origin" 2 -1 "\"origin\"" 0)
                    ("url=" 0 1 "# note" 0)
                    ("url=" 1 1 "url=" 3)
                    ("pre" 0 1 "tail" 4)
                    ("quoted" 2 1 "tail" 4)
                    ("tail" 4 -1 "pre" 0)))
      (ert-info ((format "%S" case))
        (goto-char (+ (git-ts-mode-test--position (nth 0 case)) (nth 1 case)))
        (forward-sexp (nth 2 case))
        (should (= (point) (+ (git-ts-mode-test--position (nth 3 case))
                              (nth 4 case))))))
    (goto-char (point-min))
    (forward-sexp 2)
    (should (= (point) (point-max)))
    (backward-sexp 2)
    (should (= (point) (point-min)))
    (goto-char (git-ts-mode-test--position "url="))
    (insert "other=value\n")
    (goto-char (git-ts-mode-test--position "other="))
    (forward-sexp)
    (should (= (point) (git-ts-mode-test--position "url=")))))

(ert-deftest git-ts-mode-navigates-whole-sections-including-nameless-sections ()
  (with-temp-buffer
    (setq buffer-file-name ".gitconfig")
    (insert "# head\n[core]\nx=y\n# tail\n\n[]\nz=w\n[unfinished")
    (git-ts-mode)
    (goto-char (git-ts-mode-test--position "x=y"))
    (beginning-of-defun)
    (should (= (point) (git-ts-mode-test--position "[core]")))
    (end-of-defun)
    (should (= (point) (git-ts-mode-test--position "[]")))
    (goto-char (git-ts-mode-test--position "z=w"))
    (beginning-of-defun)
    (should (= (point) (git-ts-mode-test--position "[]")))
    (end-of-defun)
    (should (= (point) (git-ts-mode-test--position "[unfinished")))
    (goto-char (point-max))
    (beginning-of-defun)
    (should (= (point) (git-ts-mode-test--position "[unfinished")))))

(ert-deftest git-ts-mode-navigates-only-attribute-macros ()
  (with-temp-buffer
    (setq buffer-file-name ".gitattributes")
    (insert "*.c text\n[attr]binary -diff\n*.h text\n[attr]\n")
    (git-ts-mode)
    (goto-char (git-ts-mode-test--position "-diff"))
    (beginning-of-defun)
    (should (= (point) (git-ts-mode-test--position "[attr]binary")))
    (end-of-defun)
    (should (= (point) (git-ts-mode-test--position "*.h")))
    (goto-char (point-max))
    (beginning-of-defun)
    (should (= (point) (git-ts-mode-test--position "[attr]\n")))))

;;;; Imenu

(ert-deftest git-ts-mode-indexes-config-section-source-names ()
  (with-temp-buffer
    (setq buffer-file-name ".gitconfig")
    (insert "[CoRe]\nx=y\n[remote \"or\\\"igin\"]\ny=z\n[remote.Origin.more]\n[remote.]\n[\"unnamed\"]\n[unfinished")
    (git-ts-mode)
    (should (equal (mapcar #'car (cdr (assoc "Section" (funcall imenu-create-index-function))))
                   '("CoRe" "remote \"or\\\"igin\"" "remote.Origin.more" "remote." "unfinished")))
    (goto-char (git-ts-mode-test--position "unfinished"))
    (delete-region (point) (point-max))
    (insert "renamed]")
    (should (equal (mapcar #'car (cdr (assoc "Section" (funcall imenu-create-index-function))))
                   '("CoRe" "remote \"or\\\"igin\"" "remote.Origin.more" "remote." "renamed")))))

(ert-deftest git-ts-mode-indexes-only-named-attribute-macros ()
  (with-temp-buffer
    (setq buffer-file-name ".gitattributes")
    (insert "[attr]binary -diff\n*.c text\n[attr]\n[attr]last attr\n\"[attr]quoted\" text\n\"\\133attr]\\146oo\" text\n")
    (git-ts-mode)
    (should (equal (mapcar #'car (cdr (assoc "Macro" (funcall imenu-create-index-function))))
                   '("binary" "last" "quoted" "\\146oo")))))

(ert-deftest git-ts-mode-provides-no-ignore-imenu-or-defun-units ()
  (with-temp-buffer
    (setq buffer-file-name ".gitignore")
    (insert "*.c\n")
    (git-ts-mode)
    (should-not treesit-simple-imenu-settings)
    (should-not (alist-get 'defun (alist-get 'gitignore treesit-thing-settings)))))

;;;; Indentation

(ert-deftest git-ts-mode-preserves-ignore-source-during-indentation ()
  (with-temp-buffer
    (setq buffer-file-name ".gitignore")
    (insert "  pattern\n \t another\n \n\t\n# comment\n")
    (git-ts-mode)
    (let ((source (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) source))
      (goto-char (point-min))
      (while (not (eobp))
        (indent-according-to-mode)
        (forward-line 1))
      (should (equal (buffer-string) source)))))

(ert-deftest git-ts-mode-indents-attributes-without-changing-token-spacing ()
  (with-temp-buffer
    (setq buffer-file-name ".gitattributes")
    (insert "  \" a b \"  attr=value\n\t[attr]binary  -diff\n  # note\n  \n")
    (git-ts-mode)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "\" a b \"  attr=value\n[attr]binary  -diff\n# note\n  \n"))
    (goto-char (point-max))
    (forward-line -1)
    (indent-according-to-mode)
    (should (= (line-beginning-position) (line-end-position)))))

(ert-deftest git-ts-mode-indents-config-but-preserves-continuation-lines ()
  (with-temp-buffer
    (setq buffer-file-name ".gitconfig")
    (insert "  [core]\n  key  =  first\\\n \t second\\\n  \"third\\\n    fourth\" ; tail\n   other = yes\n  # note\n  \n")
    (git-ts-mode)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "[core]\nkey  =  first\\\n \t second\\\n  \"third\\\n    fourth\" ; tail\nother = yes\n# note\n  \n"))
    (let ((once (buffer-string)))
      (goto-char (point-min))
      (forward-line 2)
      (indent-according-to-mode)
      (should (equal (buffer-string) once))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) once)))
    (goto-char (point-max))
    (forward-line -1)
    (indent-according-to-mode)
    (should (= (line-beginning-position) (line-end-position)))))

(ert-deftest git-ts-mode-preserves-blank-value-continuations ()
  (dolist (source '("[core]\nkey=first\\\n   |\nnext=x\n"
                    "[core]\nkey=\"first\\\n   |\"\nnext=x\n"))
    (with-temp-buffer
      (setq buffer-file-name ".gitconfig")
      (insert source)
      (git-ts-mode)
      (goto-char (git-ts-mode-test--position "|"))
      (delete-char 1)
      (indent-according-to-mode)
      (should (= (current-column) 3)))))

;;;; Updates

(ert-deftest git-ts-mode-updates-like-fresh-buffer ()
  (pcase-dolist (`(,file ,source ,old ,new)
                 '((".gitignore" "[[:digit:]]\n" "digit" "unknown")
                   (".gitignore" "# note\n" "#" "\\#")
                   (".gitattributes" "[attr]macro text\n" "[attr]" "*.c")
                   (".gitattributes" "\"[a]\" text\n" "[a]" "\\133a\\135")
                   (".gitconfig" "[core]\na=\"# note\"\n" "\"# note\"" "# note")
                   (".gitconfig" "[core]\na=value\\\n  continued\n" "\\\n" "\n")))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level 4)) (git-ts-mode))
      (git-ts-mode-test--buffer-state)
      (goto-char (git-ts-mode-test--position old))
      (delete-char (length old))
      (insert new)
      (git-ts-mode-test--should-match-fresh-buffer 4))))

(ert-deftest git-ts-mode-preserves-syntax-when-narrowed ()
  (dolist (file '(".gitignore" ".gitattributes" ".gitconfig"))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert "# head\nnext\n# tail\n")
      (git-ts-mode)
      (narrow-to-region 8 (point-max))
      (syntax-propertize (point-max))
      (should (nth 4 (syntax-ppss 16)))
      (widen)
      (should (nth 4 (syntax-ppss 4)))
      (goto-char 1)
      (delete-char 1)
      (narrow-to-region 7 (point-max))
      (syntax-propertize (point-max))
      (widen)
      (should-not (nth 4 (syntax-ppss 3))))))

(provide 'git-ts-mode-test)

;;; git-ts-mode-test.el ends here
