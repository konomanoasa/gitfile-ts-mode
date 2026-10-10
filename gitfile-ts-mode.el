;;; gitfile-ts-mode.el --- Tree-sitter mode for Git files  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 konomanoasa
;;
;; Author: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Maintainer: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages
;; URL: https://github.com/konomanoasa/gitfile-ts-mode
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

;;; Commentary:
;;
;; Tree-sitter major mode for Git files.

;;; Code:

(require 'elec-pair)
(require 'newcomment)
(require 'treesit)

(defgroup gitfile-ts nil
  "Tree-sitter mode for Git files."
  :group 'languages)

;;;; Grammar

(defconst gitfile-ts-mode--grammar-sources
  '((gitignore "https://github.com/konomanoasa/tree-sitter-gitignore"
               :revision "v0.3.0")
    (gitattributes "https://github.com/konomanoasa/tree-sitter-gitattributes"
                   :revision "v0.3.0")
    (gitconfig "https://github.com/konomanoasa/tree-sitter-gitconfig"
               :revision "v0.3.0")
    (mailmap "https://github.com/konomanoasa/tree-sitter-mailmap"
             :revision "v0.2.0"))
  "Tree-sitter grammar sources for Git files.")

(defun gitfile-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language gitfile-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

(defun gitfile-ts-mode--language ()
  "Return the Tree-sitter language selected by the file name."
  (let ((name (and buffer-file-name (file-name-nondirectory buffer-file-name))))
    (or (and name
             (cond
              ((string-suffix-p ".gitignore" name) 'gitignore)
              ((string-suffix-p ".gitattributes" name) 'gitattributes)
              ((or (string-suffix-p ".gitconfig" name)
                   (string-suffix-p ".gitmodules" name)) 'gitconfig)
              ((string-suffix-p ".mailmap" name) 'mailmap)))
        (user-error "Cannot select a Git grammar for file name %S" name))))

(defun gitfile-ts-mode--after-change (beg _end old-length)
  "Reparse a BOM moved to the start by an edit at BEG of OLD-LENGTH."
  (when (and (= beg 1) (> old-length 0))
    (save-restriction
      (widen)
      (when (eq (char-after 1) #xfeff)
        (let* ((parser treesit-primary-parser)
               (language (treesit-parser-language parser))
               (notifiers (treesit-parser-notifiers parser))
               (level (treesit-parser-embed-level parser)))
          (treesit-parser-delete parser)
          (setq treesit-primary-parser (treesit-parser-create language))
          (treesit-parser-set-embed-level treesit-primary-parser level)
          (dolist (notifier notifiers)
            (treesit-parser-add-notifier treesit-primary-parser notifier))
          (syntax-ppss-flush-cache 1)
          (font-lock-flush))))))

;;;; Syntax

(defvar gitfile-ts-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?\; ?\" ?\' ?` ?\\ ?\( ?\) ?\[ ?\] ?{ ?} ?< ?> ?: ?. ?=))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `gitfile-ts-mode'.")

;;;;; Syntax Queries

(defconst gitfile-ts-mode-syntax--queries
  (mapcar (lambda (source)
            (let ((language (car source)))
              (cons language
                    (treesit-query-compile
                     language
                     (append
                      '((comment_marker) @comment)
                      (cond
                       ((eq language 'gitconfig)
                        '((section_open) @square-open
                          (section_close) @square-close))
                       ((eq language 'mailmap)
                        '((email_open) @angle-open
                          (email_close) @angle-close))
                       (t
                        '(((character_class "[" @square-open)
                           (:equal @square-open "["))
                          ((set_open) @square-open
                           (:equal @square-open "["))
                          ((set_close) @square-close
                           (:equal @square-close "]"))
                          ((character_class "]" @square-close)
                           (:equal @square-close "]"))))))))))
          gitfile-ts-mode--grammar-sources)
  "Compiled syntax queries by language for Git files.")

;;;;; Propertization

(defun gitfile-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (remove-text-properties (point-min) start '(syntax-table nil))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        (alist-get (treesit-parser-language treesit-primary-parser)
                                   gitfile-ts-mode-syntax--queries)
                        start end))
        (let ((node (cdr capture))
              (syntax (pcase (car capture)
                        ('comment "<")
                        ('square-open "(]")
                        ('square-close ")[")
                        ('angle-open "(>")
                        ('angle-close ")<"))))
          (unless (or (= (treesit-node-start node) (treesit-node-end node))
                      (treesit-node-check node 'missing))
            (put-text-property (treesit-node-start node) (treesit-node-end node)
                               'syntax-table (string-to-syntax syntax))))))))

;;;;; Setup

(defun gitfile-ts-mode-syntax--setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function
              #'gitfile-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t))

;;;; Comment Commands

(defun gitfile-ts-mode-comment--uncomment-region (beg end &optional arg)
  "Uncomment BEG through END using syntax classified before editing.
Pass ARG to `uncomment-region-default'."
  (syntax-propertize end)
  (unwind-protect
      (let ((syntax-propertize-function nil))
        (uncomment-region-default beg end arg))
    (syntax-ppss-flush-cache beg)))

(defun gitfile-ts-mode-comment--insert-comment ()
  "Insert a standalone comment at a line boundary."
  (beginning-of-line)
  (if (looking-at-p "[ \t]*$")
      (delete-horizontal-space)
    (save-excursion (insert "\n")))
  (insert (comment-padright comment-start))
  (indent-according-to-mode))

(defun gitfile-ts-mode-comment--setup ()
  "Configure comment commands for the current buffer."
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (let ((language (treesit-parser-language treesit-primary-parser)))
    (setq-local comment-start-skip
                (pcase language
                  ('gitignore "# ?")
                  ('gitattributes "#[ \t]*")
                  ('gitconfig "[#;][ \t]*")
                  ('mailmap "#[ \t]*")))
    (setq-local comment-use-syntax t)
    (setq-local uncomment-region-function #'gitfile-ts-mode-comment--uncomment-region)
    (unless (memq language '(gitconfig mailmap))
      (setq-local comment-insert-comment-function #'gitfile-ts-mode-comment--insert-comment))
    (setq-local comment-column 0)
    (when (eq language 'gitignore)
      (setq-local comment-style 'plain))))

;;;; Electric Pair

(defun gitfile-ts-mode-electric-pair--setup ()
  "Configure electric pairing for the current buffer."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      (if (eq (treesit-parser-language treesit-primary-parser) 'mailmap)
                          '((?< . ?>))
                        '((?\[ . ?\])))
                      (when (memq (treesit-parser-language treesit-primary-parser)
                                  '(gitattributes gitconfig))
                        '((?\" . ?\")))))
  (setq-local electric-pair-open-newline-between-pairs nil))

;;;; Font Lock

;;;;; Features

(defconst gitfile-ts-mode-font-lock--feature-lists
  '((gitignore
     (comment)
     (string)
     (type escape)
     (operator constant punctuation bracket))
    (gitattributes
     (comment)
     (keyword variable string)
     (type escape)
     (operator constant punctuation bracket))
    (gitconfig
     (comment)
     (type property string)
     (escape)
     (operator punctuation bracket))
    (mailmap
     (comment)
     (string)
     ()
     (bracket)))
  "Font-lock features by language and decoration level.")

;;;;; Settings

(defun gitfile-ts-mode-font-lock--settings (language)
  "Return font-lock settings for LANGUAGE."
  (append
   (treesit-font-lock-rules
    :default-language language

    :feature 'comment
    '([(comment_marker) (comment_text)] @font-lock-comment-face))

   (cond
    ((eq language 'mailmap)
     (treesit-font-lock-rules
      :default-language language
      :feature 'string
      '([(name_text) (email_text)] @font-lock-string-face)
      :feature 'bracket
      '([(email_open) (email_close)] @font-lock-bracket-face)))
    ((eq language 'gitconfig)
     (treesit-font-lock-rules
      :default-language language

      :feature 'type
      '((section_name (name_text) @font-lock-type-face))

      :feature 'property
      '((variable_name (name_text) @font-lock-property-name-face))

      :feature 'string
      '([(subsection_text) (value_text) (quote_open) (quote_close)]
        @font-lock-string-face)

      :feature 'escape
      '([(escape) (line_continuation)] @font-lock-escape-face)

      :feature 'operator
      '((assignment_operator) @font-lock-operator-face)

      :feature 'punctuation
      '((subsection_separator) @font-lock-punctuation-face)

      :feature 'bracket
      '([(section_open) (section_close)] @font-lock-bracket-face)))
    (t
     (append
      (treesit-font-lock-rules
       :default-language language

       :feature 'string
       '([(glob_literal) (set_text) (range_character)] @font-lock-string-face)

       :feature 'type
       '((class_name) @font-lock-type-face)

       :feature 'escape
       '((escape) @font-lock-escape-face)

       :feature 'operator
       '((range_operator) @font-lock-operator-face
         (set_negation) @font-lock-negation-char-face)

       :feature 'constant
       '([(wildcard) (recursive_wildcard) (single_character)]
         @font-lock-constant-face)

       :feature 'punctuation
       '((path_separator) @font-lock-punctuation-face
         (character_class ":" @font-lock-punctuation-face))

       :feature 'bracket
       '([(set_open) (set_close)] @font-lock-bracket-face
         (character_class ["[" "]"] @font-lock-bracket-face)))

      (if (eq language 'gitignore)
          (treesit-font-lock-rules
           :default-language language

           :feature 'operator
           '((negation) @font-lock-negation-char-face))
        (treesit-font-lock-rules
         :default-language language

         :feature 'keyword
         '((macro_marker) @font-lock-keyword-face)

         :feature 'variable
         '((name_text) @font-lock-variable-name-face)

         :feature 'string
         '([(value_text) (quote_open) (quote_close)] @font-lock-string-face)

         :feature 'escape
         '((quoted_escape) @font-lock-escape-face)

         :feature 'operator
         '((assignment_operator) @font-lock-operator-face
           [(unset_marker) (unspecified_marker)] @font-lock-negation-char-face))))))))

;;;;; Setup

(defun gitfile-ts-mode-font-lock--setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list
              (alist-get (treesit-parser-language treesit-primary-parser)
                         gitfile-ts-mode-font-lock--feature-lists))
  (setq-local treesit-font-lock-settings
              (gitfile-ts-mode-font-lock--settings
               (treesit-parser-language treesit-primary-parser))))

;;;; Navigation

(defconst gitfile-ts-mode-navigation--settings
  `((gitignore (sexp "^pattern$"))
    (gitattributes
     (sexp ,(rx string-start
                (or "attribute_rule" "macro_definition" "pattern" "attribute"
                    "attribute_name" "attribute_value")
                string-end))
     (defun "^macro_definition$"))
    (gitconfig
     (sexp ,(rx string-start
                (or "section" "variable" "section_name" "subsection"
                    "variable_name" "value")
                string-end))
     (defun "^section$"))
    (mailmap
     (sexp ,(rx string-start (or "mapping" "name" "email") string-end))
     (defun "^mapping$")))
  "Tree-sitter thing definitions for Git files.")

(defun gitfile-ts-mode-navigation--setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings
              gitfile-ts-mode-navigation--settings)
  (setq-local treesit-defun-skipper nil))

;;;; Imenu

(defun gitfile-ts-mode-imenu--name (node)
  "Return the source name of NODE, or nil if it has no name."
  (pcase (treesit-node-type node)
    ("section"
     (let* ((header (treesit-node-child-by-field-name node "header"))
            (name (and header (treesit-node-child-by-field-name header "name")))
            (subsection (and header (treesit-node-child-by-field-name
                                     header "subsection"))))
       (when name
         (concat (treesit-node-text name t)
                 (when subsection
                   (concat
                    (if (equal (treesit-node-type (treesit-node-child subsection 0))
                               "subsection_separator") "" " ")
                    (treesit-node-text subsection t)))))))
    ("macro_definition"
     (let ((name (treesit-node-child-by-field-name node "name")))
       (when name (treesit-node-text name t))))
    ("mapping"
     (let ((name (or (treesit-node-child-by-field-name node "canonical_name")
                     (treesit-node-child-by-field-name node "canonical_email"))))
       (when name (treesit-node-text name t))))))

(defconst gitfile-ts-mode-imenu--settings
  '((gitattributes
     ("Macro" "^macro_definition$" gitfile-ts-mode-imenu--name nil))
    (gitconfig
     ("Section" "^section$" gitfile-ts-mode-imenu--name nil))
    (mailmap
     ("Mapping" "^mapping$" gitfile-ts-mode-imenu--name nil)))
  "Tree-sitter Imenu settings by language for Git files.")

(defun gitfile-ts-mode-imenu--setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function
              #'gitfile-ts-mode-imenu--name)
  (setq-local treesit-simple-imenu-settings
              (alist-get (treesit-parser-language treesit-primary-parser)
                         gitfile-ts-mode-imenu--settings)))

;;;; Indentation

(defun gitfile-ts-mode-indent--continuation-p (node parent bol)
  "Return non-nil if NODE or PARENT at BOL is on a continued variable line."
  (let ((variable (treesit-parent-until (or node parent) "^variable$" t)))
    (and variable
         (< (treesit-node-start variable)
            (save-excursion (goto-char bol) (line-beginning-position))))))

(defun gitfile-ts-mode-indent--keep (_node _parent bol)
  "Return BOL to preserve the current indentation."
  bol)

(defconst gitfile-ts-mode-indent--rules
  '((gitignore (catch-all gitfile-ts-mode-indent--keep 0))
    (gitattributes (catch-all column-0 0))
    (gitconfig (gitfile-ts-mode-indent--continuation-p gitfile-ts-mode-indent--keep 0)
               (catch-all column-0 0))
    (mailmap (catch-all column-0 0)))
  "Tree-sitter indentation rules for Git files.")

(defun gitfile-ts-mode-indent--setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              gitfile-ts-mode-indent--rules))

;;;; Mode

(defun gitfile-ts-mode--setup ()
  "Configure `gitfile-ts-mode' in the current buffer."
  (let ((language (gitfile-ts-mode--language)))
    (gitfile-ts-mode--ensure-grammar language)
    (setq-local treesit-primary-parser (treesit-parser-create language)))
  (gitfile-ts-mode-syntax--setup)
  (gitfile-ts-mode-comment--setup)
  (gitfile-ts-mode-electric-pair--setup)
  (gitfile-ts-mode-font-lock--setup)
  (gitfile-ts-mode-navigation--setup)
  (gitfile-ts-mode-imenu--setup)
  (gitfile-ts-mode-indent--setup)
  (treesit-major-mode-setup)
  (add-hook 'after-change-functions #'gitfile-ts-mode--after-change nil t))

;;;###autoload
(define-derived-mode gitfile-ts-mode prog-mode "Gitfile-TS"
  "Major mode for editing Git files."
  :syntax-table gitfile-ts-mode-syntax-table
  :group 'gitfile-ts
  (gitfile-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (rx (or (seq ".git" (or "attributes" "config" "ignore" "modules"))
                           ".mailmap")
                       string-end)
                   'gitfile-ts-mode))

(provide 'gitfile-ts-mode)

;;; gitfile-ts-mode.el ends here
