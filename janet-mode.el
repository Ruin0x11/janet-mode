;;; janet-mode.el --- Janet editing mode               -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Ruin0x11

;; Author:  <ipickering2@gmail.com>
;; Keywords: languages, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing Janet. Various things are taken from
;; `clojure-mode.'

;;; Code:

(require 'lisp-mode)
(require 'prog-mode)


;;; Customization

(defgroup janet nil
  "Major mode for editing Janet code."
  :prefix "janet-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/Ruin0x11/janet-mode")
  :link '(emacs-commentary-link :tag "Commentary" "janet-mode"))

(defcustom janet-docstring-fill-prefix-width 2
  "Width of `fill-prefix' when filling a docstring.
The default value conforms with the de facto convention for Janet
docstrings, aligning the second line with the opening double
quotes on the third column."
  :type 'integer
  :safe 'integerp)

(defcustom janet-docstring-fill-column fill-column
  "Value of `fill-column' to use when filling a docstring."
  :type 'integer
  :safe 'integerp)

(defface janet-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to font-lock Clojure keywords (:something).")

(defface janet-character-face
  '((t (:inherit font-lock-string-face)))
  "Face used to font-lock Clojure character literals.")


;;; janet-mode setup

(defconst janet--sym-regexp
  "\\(\\sw\\|[-?<>*+*%=]\\)+")

(defvar janet-imenu-generic-expression
      `((nil
         ,(concat "^(\\(defn\\)*\\s-+\\((?" janet--sym-regexp "\\)") 2)
        ("Macros"
         ,(concat "^(\\(defmacro\\)\\s-+\\((?" janet--sym-regexp "\\)") 2))
  "Imenu generic expression for Janet mode.  See `imenu-generic-expression'.")

(defvar janet-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    map))

(defvar janet-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\; "'" table)
    table)
  "Syntax table for Janet mode.
Inherits from `emacs-lisp-mode-syntax-table'.")

(defun janet-mode-variables ()
  "Set up initial buffer-local variables for Clojure mode."
  (setq-local imenu-generic-expression janet-imenu-generic-expression)
  (setq-local indent-tabs-mode nil)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local outline-regexp "###\\(#* [^ \t\n]\\)\\|(")
  (setq-local outline-level 'lisp-outline-level)
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local electric-pair-open-newline-between-pairs nil)
  (setq-local fill-paragraph-function #'janet-fill-paragraph)
  (setq-local adaptive-fill-function #'janet-adaptive-fill-function)
  (setq-local normal-auto-fill-function #'janet-auto-fill-function)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(#+\\) *")
  (setq-local indent-line-function #'janet-indent-line)
  (setq-local indent-region-function #'janet-indent-region)
  (setq-local lisp-indent-function #'janet-indent-function)
  (setq-local lisp-doc-string-elt-property 'janet-doc-string-elt)
  ; (setq-local clojure-expected-ns-function #'clojure-expected-ns)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local beginning-of-defun-function #'janet-beginning-of-defun-function))

(defsubst janet-in-docstring-p ()
  "Check whether point is in a docstring."
  (let ((ppss (syntax-ppss)))
    ;; are we in a string?
    (when (nth 3 ppss)
      ;; check font lock at the start of the string
      (eq (get-text-property (nth 8 ppss) 'face)
          'font-lock-doc-face))))

(defun janet-mode-hooks ()
  ;; `electric-layout-post-self-insert-function' prevents indentation in strings
  ;; and comments, force indentation in docstrings:
  (add-hook 'electric-indent-functions
            (lambda (_char) (if (janet-in-docstring-p) 'do-indent)))
  )

;;;###autoload
(define-derived-mode janet-mode prog-mode "Janet"
  "Major mode for editing Janet code.

\\{janet-mode-map}"
  (janet-mode-variables)
  (janet-font-lock-setup)
  (janet-mode-hooks))


;;; Janet fill-function
;; borrowed from clojure-mode

(defsubst janet-docstring-fill-prefix ()
  "The prefix string used by `janet-fill-paragraph'.
It is simply `janet-docstring-fill-prefix-width' number of spaces."
  (make-string janet-docstring-fill-prefix-width ? ))

(defun janet-adaptive-fill-function ()
  "Clojure adaptive fill function.
This only takes care of filling docstring correctly."
  (when (janet-in-docstring-p)
    (janet-docstring-fill-prefix)))

(defun janet-fill-paragraph (&optional justify)
  "Like `fill-paragraph', but can handle Janet docstrings.
If JUSTIFY is non-nil, justify as well as fill the paragraph."
  (if (janet-in-docstring-p)
      (let ((paragraph-start
             (concat paragraph-start
                     "\\|\\s-*\\([(:\"[]\\|~@\\|`(\\|#'(\\)"))
            (paragraph-separate
             (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
            (fill-column (or janet-docstring-fill-column fill-column))
            (fill-prefix (janet-docstring-fill-prefix)))
        ;; we are in a string and string start pos (8th element) is non-nil
        (let* ((beg-doc (nth 8 (syntax-ppss)))
               (end-doc (save-excursion
                          (goto-char beg-doc)
                          (or (ignore-errors (forward-sexp) (point))
                              (point-max)))))
          (save-restriction
            (narrow-to-region beg-doc end-doc)
            (fill-paragraph justify))))
    (let ((paragraph-start (concat paragraph-start
                                   "\\|\\s-*\\([(:\"[]\\|`(\\|#'(\\)"))
          (paragraph-separate
           (concat paragraph-separate "\\|\\s-*\".*[,\\.[]$")))
      (or (fill-comment-paragraph justify)
          (fill-paragraph justify))
      ;; Always return `t'
      t)))

(defun janet-auto-fill-function ()
  "Janet auto-fill function."
  ;; Check if auto-filling is meaningful.
  (let ((fc (current-fill-column)))
    (when (and fc (> (current-column) fc))
      (let ((fill-column (if (janet-in-docstring-p)
                             janet-docstring-fill-column
                           fill-column))
            (fill-prefix (janet-adaptive-fill-function)))
        (do-auto-fill)))))


;;; (comment) macro formatting
;; borrowed from clojure-mode

(defconst janet--comment-macro-regexp
  "\\(?1:(comment\\_>\\)"
  "Regexp matching the (comment) macro.")

(defun janet--search-comment-macro-internal (limit)
  "Search for a comment forward stopping at LIMIT."
  (when (search-forward-regexp janet-comment-macro-regexp limit t)
    (let* ((md (match-data))
           (start (match-beginning 1))
           (state (syntax-ppss start)))
      ;; inside string or comment?
      (if (or (nth 3 state)
              (nth 4 state))
          (janet--search-comment-macro-internal limit)
        (goto-char start)
        (janet-forward-logical-sexp 1)
        ;; Data for (match-end 1).
        (setf (elt md 3) (point))
        (set-match-data md)
        t))))

(defun janet--search-comment-macro (limit)
  "Find comment macros and set the match data.
Search from point up to LIMIT.  The region that should be
considered a comment is between `(match-beginning 1)'
and `(match-end 1)'."
  (let ((result 'retry))
    (while (and (eq result 'retry) (<= (point) limit))
      (condition-case nil
          (setq result (janet--search-comment-macro-internal limit))
        (end-of-file (setq result nil))
        (scan-error  (setq result 'retry))))
    result))


;;; General font-locking
(defconst janet-font-lock-keywords
  (eval-when-compile
    `( ;; Top-level variable definition
      (,(concat "\\(" (regexp-opt '("def" "var" "defglobal"))
                "\\(-\\)?"
                ;; variable declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                "\\(\\sw+[ \t]*\\)?"
                (concat "\\(:" janet--sym-regexp "\\)?"))

       (1 font-lock-keyword-face)
       (3 font-lock-variable-name-face nil t)
       (4 'janet-keyword-face nil t))
      ;; Function definition

      (,(concat
                "\\(def[^ \r\n\t]*\\)"
                ;; Function declarations
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                (concat "\\(" janet--sym-regexp "\\)?"))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))

      ;; (fn name? args ...)
      (,(concat "\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#?^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(\\sw+\\)?" )
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))

      ;; Special forms
      (,(concat
         "("
         (regexp-opt
          '("def" "do" "if" "let" "var" "fn" "set" "get" "put" "quote" "type"
            "resume" "yield" "debug" "dyn" "setdyn" "trace" "untrace" "gensym"
            "gccollect" "gcsetinterval" "gcinterval" "next" "hash" "getline"
            ) t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Types
      (,(concat
         "("
         (regexp-opt
          '("native" "describe" "string" "symbol" "keyword" "buffer" "table" "array" "tuple" "struct") t)
         "\\>")
       1 font-lock-builtin-face)
      ;; Built-in binding and flow of control forms
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("case" "cond"
            "for" "when" "when-not" "when-let" "when-first" "when-some"
            "each" "if-let" "if-not" "if-some" "length"
            ".." "->" "->>" "as->" "doto" "and" "or" "unless"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "ns" "in-ns" "try" "while" "loop" "with-syms" "with-dyns"
            ) t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Macros similar to let, when, and while
      (,(rx symbol-start
            (or "let" "when" "while") "-"
            (1+ (or (syntax word) (syntax symbol)))
            symbol-end)
       0 font-lock-keyword-face)
      (,(concat
         "\\<"
         (regexp-opt
          '("*1" "*2" "*3" "*agent*"
            "*allow-unresolved-vars*" "*assert*" "*clojure-version*"
            "*command-line-args*" "*compile-files*"
            "*compile-path*" "*data-readers*" "*default-data-reader-fn*"
            "*e" "*err*" "*file*" "*flush-on-newline*"
            "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
            "*print-dup*" "*print-length*" "*print-level*"
            "*print-meta*" "*print-readably*"
            "*read-eval*" "*source-path*"
            "*unchecked-math*"
            "*use-context-classloader*" "*warn-on-reflection*")
          t)
         "\\>")
       0 font-lock-builtin-face)
      ;; Dynamic variables - *something* or @*something*
      (,(concat "\\(?:\\<\\|/\\)@?\\(\\*" janet--sym-regexp "\\*\\)\\>")
       1 font-lock-variable-name-face)
      ;; Global constants - nil, true, false
      (,(concat
         "\\<"
         (regexp-opt
          '("true" "false" "nil") t)
         "\\>")
       0 font-lock-constant-face)
      (,(concat
         "\\<"
         (regexp-opt
          '("error") t)
         "\\>")
       0 font-lock-warning-face)
      ;; Character literals - \1, \a, \newline, \u0000
      ("\\\\\\([[:punct:]]\\|[a-z0-9]+\\>\\)" 0 'janet-character-face)

      ;; namespace definitions: (ns foo.bar)
      (,(concat "(\\<ns\\>[ \r\n\t]*"
                ;; Possibly metadata, shorthand and/or longhand
                "\\(?:\\^?\\(?:{[^}]+}\\|:[^ \r\n\t]+[ \r\n\t]\\)[ \r\n\t]*\\)*"
                ;; namespace
                "\\(" janet--sym-regexp "\\)")
       (1 font-lock-type-face))

      ;; TODO dedupe the code for matching of keywords, type-hints and unmatched symbols

      ;; keywords: {:oneword/ve/yCom|pLex.stu-ff 0}
      (,(concat "\\(:\\{1,2\\}\\)\\(" janet--sym-regexp "?\\)\\(/\\)\\(" janet--sym-regexp "\\)")
       (1 'janet-keyword-face)
       (2 font-lock-type-face)
       ;; (2 'janet-keyword-face)
       (3 'default)
       (4 'janet-keyword-face))
      (,(concat "\\(:\\{1,2\\}\\)\\(" janet--sym-regexp "\\)")
       (1 'janet-keyword-face)
       (2 'janet-keyword-face))

      ;; type-hints: #^oneword
      (,(concat "\\(#?\\^\\)\\(" janet--sym-regexp "?\\)\\(/\\)\\(" janet--sym-regexp "\\)")
       (1 'default)
       (2 font-lock-type-face)
       (3 'default)
       (4 'default))
      (,(concat "\\(#?\\^\\)\\(" janet--sym-regexp "\\)")
       (1 'default)
       (2 font-lock-type-face))

      ;; clojure symbols not matched by the previous regexps; influences CIDER's
      ;; dynamic syntax highlighting (CDSH). See https://git.io/vxEEA:
      (,(concat "\\(" janet--sym-regexp "?\\)\\(/\\)\\(" janet--sym-regexp "\\)")
       (1 font-lock-type-face)
       ;; 2nd and 3th matching groups can be font-locked to `nil' or `default'.
       ;; CDSH seems to kick in only for functions and variables referenced w/o
       ;; writing their namespaces.
       (2 nil)
       (3 nil))
      (,(concat "\\(" janet--sym-regexp "\\)")
       ;; this matching group must be font-locked to `nil' otherwise CDSH breaks.
       (1 nil))

      ;; #_ and (comment ...) macros.
      (janet--search-comment-macro 1 font-lock-comment-face t)
      ;; Highlight `code` marks, just like `elisp'.
      (,(rx "`" (group-n 1 (optional "#'")
                         (+ (or (syntax symbol) (syntax word)))) "`")
       (1 'font-lock-constant-face prepend))
      ;; Highlight escaped characters in strings.
      ; (clojure-font-lock-escaped-chars 0 'bold prepend)
      ;; Highlight grouping constructs in regular expressions
      ; (clojure-font-lock-regexp-groups
      ;  (1 'font-lock-regexp-grouping-construct prepend)
))
"Default expressions to highlight in Janet mode.")


(defun janet-font-lock-setup ()
  "Configures font-lock for editing Janet code."
  (setq-local font-lock-multiline t)
  (add-to-list 'font-lock-extend-region-functions
               #'janet-font-lock-extend-region-def t)
  (setq font-lock-defaults
        '(janet-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&:" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          ; (font-lock-syntactic-face-function
          ;  . janet-font-lock-syntactic-face-function)
)))

(defun janet-font-lock-def-at-point (point)
  "Range between the top-most def* and the fourth element after POINT.
Note that this means that there is no guarantee of proper font
locking in def* forms that are not at top level."
  (goto-char point)
  (ignore-errors
    (beginning-of-defun))

  (let ((beg-def (point)))
    (when (and (not (= point beg-def))
               (looking-at "(def"))
      (ignore-errors
        ;; move forward as much as possible until failure (or success)
        (forward-char)
        (dotimes (_ 4)
          (forward-sexp)))
      (cons beg-def (point)))))

(defun janet-font-lock-extend-region-def ()
  "Set region boundaries to include the first four elements of def* forms."
  (let ((changed nil))
    (let ((def (janet-font-lock-def-at-point font-lock-beg)))
      (when def
        (cl-destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-beg)
                     (< font-lock-beg def-end))
            (setq font-lock-beg def-beg
                  changed t)))))
    (let ((def (janet-font-lock-def-at-point font-lock-end)))
      (when def
        (cl-destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-end)
                     (< font-lock-end def-end))
            (setq font-lock-end def-end
                  changed t)))))
    changed))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.janet\\'" . janet-mode)))

(provide 'janet-mode)
;;; janet-mode.el ends here
