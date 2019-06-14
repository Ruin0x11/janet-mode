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

(require 'inf-lisp)
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

(defcustom janet-indent-style 'always-indent
  "Indentation style to use for function forms and macro forms.
There are two cases of interest configured by this variable.

- Case (A) is when at least one function argument is on the same
  line as the function name.
- Case (B) is the opposite (no arguments are on the same line as
  the function name).  Note that the body of macros is not
  affected by this variable, it is always indented by
  `lisp-body-indent' (default 2) spaces.

Note that this variable configures the indentation of function
forms (and function-like macros), it does not affect macros that
already use special indentation rules.

The possible values for this variable are keywords indicating how
to indent function forms.

    `always-align' - Follow the same rules as `lisp-mode'.  All
    args are vertically aligned with the first arg in case (A),
    and vertically aligned with the function name in case (B).
    For instance:
        (reduce merge
                some-coll)
        (reduce
         merge
         some-coll)

    `always-indent' - All args are indented like a macro body.
        (reduce merge
          some-coll)
        (reduce
          merge
          some-coll)

    `align-arguments' - Case (A) is indented like `lisp', and
    case (B) is indented like a macro body.
        (reduce merge
                some-coll)
        (reduce
          merge
          some-coll)"
  :safe #'symbolp
  :type '(choice (const :tag "Same as `lisp-mode'" 'always-align)
                 (const :tag "Indent like a macro body" 'always-indent)
                 (const :tag "Indent like a macro body unless first arg is on the same line"
                        'align-arguments)))

(defcustom janet-use-backtracking-indent t
  "When non-nil, enable context sensitive indentation."
  :type 'boolean
  :safe 'booleanp)

(defcustom janet-max-backtracking 3
  "Maximum amount to backtrack up a list to check for context."
  :type 'integer
  :safe 'integerp)

(defcustom janet-docstring-fill-column fill-column
  "Value of `fill-column' to use when filling a docstring."
  :type 'integer
  :safe 'integerp)

(defcustom janet-docstring-fill-prefix-width 2
  "Width of `fill-prefix' when filling a docstring.
The default value conforms with the de facto convention for
Janet docstrings, aligning the second line with the opening
double quotes on the third column."
  :type 'integer
  :safe 'integerp)


;;; REPL interaction
;; mostly from python-mode

(defun janet-repl-get-process ()
  "Return inferior Janet process for current buffer."
  (get-buffer-process inferior-lisp-buffer))

(defun janet-repl-get-process-or-error (&optional interactivep)
  "Return inferior Python process for current buffer or signal error.
When argument INTERACTIVEP is non-nil, use `user-error' instead
of `error' with a user-friendly message."
  (or (janet-repl-get-process)
      (if interactivep
          (user-error
           "Start a Janet process first with `M-x run-python' or `%s'."
           ;; Get the binding.
           (key-description
            (where-is-internal
             #'run-lisp overriding-local-map t)))
        (error
         "No inferior Janet process running."))))

(defvar janet-repl-output-filter-in-progress nil)
(defvar janet-repl-output-filter-buffer nil)

(defun janet-repl-comint-end-of-output-p (output)
  "Return non-nil if OUTPUT ends with input prompt."
  (string-match
   ;; XXX: It seems on macOS an extra carriage return is attached
   ;; at the end of output, this handles that too.
   (concat
    "\r?\n?"
    ;; Remove initial caret from calculated regexp
    (replace-regexp-in-string
     (rx string-start ?^) ""
     inferior-lisp-prompt)
    (rx eos))
   output))

(defun janet-repl-output-filter (string)
  "Filter used in `janet-repl-send-string-no-output' to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`janet-repl-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (setq
   string (ansi-color-filter-apply string)
   janet-repl-output-filter-buffer
   (concat janet-repl-output-filter-buffer string))
  (when (janet-repl-comint-end-of-output-p
         janet-repl-output-filter-buffer)
    ;; Output ends when `janet-repl-output-filter-buffer' contains
    ;; the prompt attached at the end of it.
    (setq janet-repl-output-filter-in-progress nil
          janet-repl-output-filter-buffer
          (substring janet-repl-output-filter-buffer
                     0 (match-beginning 0)))
    (when (string-match
           inferior-lisp-prompt
           janet-repl-output-filter-buffer)
      ;; Some shells, like IPython might append a prompt before the
      ;; output, clean that.
      (setq janet-repl-output-filter-buffer
            (substring janet-repl-output-filter-buffer (match-end 0)))))
  "")

(defun janet-repl-send-string (string &optional process msg)
  "Send STRING to inferior Janet PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list (read-string "Eval: ") nil t))
  (let ((process (or process (janet-repl-get-process-or-error msg))))
      (comint-send-string process string)
      (when (or (not (string-match "\n\\'" string))
                (string-match "\n[ \t].*\n?\\'" string))
        (comint-send-string process "\n"))))

(defun janet-repl-send-string-no-output (str &optional proc)
  (let ((proc (or proc (janet-repl-get-process)))
        (comint-preoutput-filter-functions
         '(janet-repl-output-filter))
        (janet-repl-output-filter-in-progress t)
        (inhibit-quit t))
    (or
     (with-local-quit
       (janet-repl-send-string str proc)
       (while janet-repl-output-filter-in-progress
         ;; `janet-repl-output-filter' takes care of setting
         ;; `janet-repl-output-filter-in-progress' to NIL after it
         ;; detects end of output.
         (accept-process-output proc))
       (prog1
           janet-repl-output-filter-buffer
         (setq janet-repl-output-filter-buffer nil)))
     (with-current-buffer (process-buffer proc)
       (comint-interrupt-subjob)))))

(defun janet-eval-expression (exp)
  "Send STRING to inferior Janet PROCESS.
Open Janet buffer if not open."
  (interactive
   (list (read-string "Eval: ")))
  (janet-repl-send-string exp)
  (let ((pop-up-frames
         ;; Be willing to use another frame
         ;; that already has the window in it.
         (or pop-up-frames
             (get-buffer-window inferior-lisp-buffer t))))
    (pop-to-buffer inferior-lisp-buffer nil t))
  )

(defun janet-show-doc (sym)
  )

(defun janet-doc (arg)
  "Run `doc' for the method at point."
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (janet-show-doc
     (if (or (not thing) arg)
         (read-string "Symbol: ")
       thing))))


;;; janet-mode setup

(defconst janet--sym-regexp
  "\\(\\sw\\|[-?<>*+*%=]\\)+")

(defvar janet-imenu-generic-expression
  `((nil
     ,(concat "^(\\(defn\\)*\\s-+\\((?" janet--sym-regexp "\\)") 2)
    ("Macros"
     ,(concat "^(\\(defmacro\\)\\s-+\\((?" janet--sym-regexp "\\)") 2))
  "Imenu generic expression for Janet mode.  See `imenu-generic-expression'.")

(defun janet-eval-buffer (&optional and-go)
  (interactive)
  (lisp-eval-region (point-min) (point-max) and-go))

(defvar janet-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map "\M-\C-x"  'lisp-eval-defun)     ; GNU convention
    (define-key map "\C-x\C-e" 'lisp-eval-last-sexp) ; GNU convention
    (define-key map "\C-c\C-e" 'lisp-eval-defun)
    (define-key map "\C-c\C-r" 'lisp-eval-region)
    (define-key map "\C-c\C-n" 'lisp-eval-form-and-next)
    (define-key map "\C-c\C-p" 'lisp-eval-paragraph)
    (define-key map "\C-c\C-l" 'janet-eval-buffer)
    (define-key map "\C-c\C-c" 'lisp-compile-defun)
    (define-key map "\C-c\C-z" 'switch-to-lisp)
                                        ; (define-key janet-mode-map "\C-c\C-l" 'lisp-load-file)
                                        ; (define-key janet-mode-map "\C-c\C-a" 'lisp-show-arglist)
    (define-key map "\C-c\C-d" 'lisp-describe-sym)
    (define-key map "\C-c\C-f" 'lisp-show-function-documentation)
    (define-key map "\C-c\C-v" 'lisp-show-variable-documentation)
    map))

(defvar janet-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?@ "_ p" table)
    (modify-syntax-entry ?~ "_ p" table)
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
  (setq-local comment-add 1) ; default to `##' in comment-region
  (setq-local comment-column 0)
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
                                        ;(setq-local beginning-of-defun-function #'janet-beginning-of-defun-function)
  (setq-local inferior-lisp-program "janet -s")
  (setq-local inferior-lisp-prompt "janet:[:digit:]+: ")
  (setq-local comint-prompt-regexp "janet:[:digit:]+: ")
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-inhibit-carriage-motion t)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-process-echoes t)
  (setq-local mode-line-process '(":%s"))
)

(defsubst janet-in-docstring-p ()
  "Check whether point is in a docstring."
  (let ((ppss (syntax-ppss)))
    ;; are we in a string?
    (when (nth 3 ppss)
      ;; check font lock at the start of the string
      (eq (get-text-property (nth 8 ppss) 'face)
          'font-lock-doc-face))))

(defun janet-comint-output-filter-function (output)
  "Hook run after content is put into comint buffer.
   OUTPUT is a string with the contents of the buffer"
  (ansi-color-filter-apply output))

(defun janet-mode-hooks ()
  ;; `electric-layout-post-self-insert-function' prevents indentation in strings
  ;; and comments, force indentation in docstrings:
  (make-local-variable 'electric-indent-functions)
  (add-hook 'electric-indent-functions
            (lambda (_char) (if (janet-in-docstring-p) 'do-indent)))
  (make-local-variable 'comint-output-filter-functions)
  (add-hook 'comint-output-filter-functions
            'janet-comint-output-filter-function)
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
                     "\\|\\s-*\\([(:\"[]\\|~@\\|`(\\|\\)"))
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
                                   "\\|\\s-*\\([(:\"[]\\|`(\\|\\)"))
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
        (forward-sexp 1)
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
      (,(concat "\\<\\(" (regexp-opt '("def" "var" "defglobal"))
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
         "\\<\\(" (regexp-opt '("defn" "defmacro"))
         "\\(-\\)?"
         ;; Function declarations
         "\\)\\>"
         ;; Any whitespace
         "[ \r\n\t]*"
         (concat "\\(" janet--sym-regexp "\\)?"))
       (1 font-lock-keyword-face)
       (3 font-lock-function-name-face nil t))

      ;; (fn name? args ...)
      (,(concat "\\<\\(fn\\)[ \t]+"
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
            "asm" "import" "require" "dofile" "apply") t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Types
      (,(concat
         "("
         (regexp-opt
          '("native" "describe" "doc" "string" "symbol" "keyword" "buffer" "table" "array" "tuple" "struct") t)
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
      (,(concat "```[^`]*```")
       0 font-lock-string-face t t t)
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

      ;; (comment ...) macros.
      (janet--search-comment-macro 1 font-lock-comment-face t)
      ;; Highlight `code` marks, just like `elisp'.
      (,(rx "`" (group-n 1
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
          (font-lock-syntactic-face-function
           . janet-font-lock-syntactic-face-function))))

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

(defun janet-font-lock-syntactic-face-function (state)
  "Find and highlight text with a Janet-friendly syntax table.

This function is passed to `font-lock-syntactic-face-function',
which is called with a single parameter, STATE (which is, in
turn, returned by `parse-partial-sexp' at the beginning of the
highlighted region)."
  (if (nth 3 state)
      ;; This might be a (doc)string or a |...| symbol.
      (let ((startpos (nth 8 state)))
        (if (eq (char-after startpos) ?|)
            ;; This is not a string, but a |...| symbol.
            nil
          (let* ((listbeg (nth 1 state))
                 (firstsym (and listbeg
                                (save-excursion
                                  (goto-char listbeg)
                                  (and (looking-at "([ \t\n]*\\(\\(\\sw\\|\\s_\\)+\\)")
                                       (match-string 1)))))
                 (docelt (and firstsym
                              (function-get (intern-soft firstsym)
                                            lisp-doc-string-elt-property))))
            (if (and docelt
                     ;; It's a string in a form that can have a docstring.
                     ;; Check whether it's in docstring position.
                     (save-excursion
                       (when (functionp docelt)
                         (goto-char (match-end 1))
                         (setq docelt (funcall docelt)))
                       (goto-char listbeg)
                       (forward-char 1)
                       (ignore-errors
                         (while (and (> docelt 0) (< (point) startpos)
                                     (progn (forward-sexp 1) t))
                           ;; ignore metadata and type hints
                           (unless (looking-at "[ \n\t]*\\(\\^[A-Z:].+\\|\\^?{.+\\)")
                             (setq docelt (1- docelt)))))
                       (and (zerop docelt) (<= (point) startpos)
                            (progn (forward-comment (point-max)) t)
                            (= (point) (nth 8 state))))
                     ;; In a def, at last position is not a docstring
                     (not (and (string= "def" firstsym)
                               (save-excursion
                                 (goto-char startpos)
                                 (goto-char (+ startpos (length (sexp-at-point)) 2))
                                 (looking-at "[ \r\n\t]*\)")))))
                font-lock-doc-face
              font-lock-string-face))))
    font-lock-comment-face))

;; Docstring positions
(put 'def 'janet-doc-string-elt 2)
(put 'def- 'janet-doc-string-elt 2)
(put 'defn 'janet-doc-string-elt 2)
(put 'defn- 'janet-doc-string-elt 2)
(put 'defmacro 'janet-doc-string-elt 2)
(put 'defmacro- 'janet-doc-string-elt 2)


;;; Vertical alignment
(defcustom janet-align-forms-automatically nil
  "If non-nil, vertically align some forms automatically.
Automatically means it is done as part of indenting code.  This
applies to binding forms (`janet-align-binding-forms'), to cond
forms (`janet-align-cond-forms') and to map literals.  For
instance, selecting a map a hitting \\<janet-mode-map>`\\[indent-for-tab-command]'
will align the values like this:
    {:some-key 10
     :key2     20}"
  :safe #'booleanp
  :type 'boolean)

(defconst janet--align-separator-newline-regexp "^ *$")

(defcustom janet-align-separator janet--align-separator-newline-regexp
  "The separator that will be passed to `align-region' when performing vertical alignment."
  :type `(choice (const :tag "Make blank lines prevent vertical alignment from happening."
                        ,janet--align-separator-newline-regexp)
                 (other :tag "Allow blank lines to happen within a vertically-aligned expression."
                        'entire)))

(defcustom janet-align-binding-forms
  '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop"
    "doseq" "for" "with-open" "with-local-vars" "with-redefs")
  "List of strings matching forms that have binding forms."
  :safe #'listp
  :type '(repeat string))

(defcustom janet-align-cond-forms '("cond" "case" "are")
  "List of strings identifying cond-like forms."
  :safe #'listp
  :type '(repeat string))

(defun janet--position-for-alignment ()
  "Non-nil if the sexp around point should be automatically aligned.
This function expects to be called immediately after an
open-brace or after the function symbol in a function call.

First check if the sexp around point is a map literal, or is a
call to one of the vars listed in `janet-align-cond-forms'.  If
it isn't, return nil.  If it is, return non-nil and place point
immediately before the forms that should be aligned.

For instance, in a map literal point is left immediately before
the first key; while, in a let-binding, point is left inside the
binding vector and immediately before the first binding
construct."
  (let ((point (point)))
    ;; Are we in a map?
    (or (and (eq (char-before) ?{)
             (not (eq (char-before (1- point)) ?\@)))
        ;; Are we in a cond form?
        (let* ((fun    (car (member (thing-at-point 'symbol) janet-align-cond-forms)))
               (method (and fun (janet--get-indent-method fun)))
               ;; The number of special arguments in the cond form is
               ;; the number of sexps we skip before aligning.
               (skip   (cond ((numberp method) method)
                             ((null method) 0)
                             ((sequencep method) (elt method 0)))))
          (when (and fun (numberp skip))
            (forward-sexp skip)
            (comment-forward (point-max))
            fun)) ; Return non-nil (the var name).
        ;; Are we in a let-like form?
        (when (member (thing-at-point 'symbol)
                      janet-align-binding-forms)
          ;; Position inside the binding vector.
          (forward-sexp)
          (backward-sexp)
          (when (eq (char-after) ?\[)
            (forward-char 1)
            (comment-forward (point-max))
            ;; Return non-nil.
            t)))))

(defun janet--find-sexp-to-align (end)
  "Non-nil if there's a sexp ahead to be aligned before END.
Place point as in `janet--position-for-alignment'."
  ;; Look for a relevant sexp.
  (let ((found))
    (while (and (not found)
                (search-forward-regexp
                 (concat "{\\|("
                         (regexp-opt
                          (append janet-align-binding-forms
                                  janet-align-cond-forms)
                          'symbols))
                 end 'noerror))

      (let ((ppss (syntax-ppss)))
        ;; If we're in a string or comment.
        (unless (or (elt ppss 3)
                    (elt ppss 4))
          ;; Only stop looking if we successfully position
          ;; the point.
          (setq found (janet--position-for-alignment)))))
    found))

(defun janet--search-whitespace-after-next-sexp (&optional bound _noerror)
  "Move point after all whitespace after the next sexp.

Set the match data group 1 to be this region of whitespace and
return point.

BOUND is bounds the whitespace search."
  (unwind-protect
      (ignore-errors
        (forward-sexp 1)
        (search-forward-regexp "\\([,\s\t]*\\)" bound)
        (pcase (syntax-after (point))
          ;; End-of-line, try again on next line.
          (`(12) (janet--search-whitespace-after-next-sexp bound))
          ;; Closing paren, stop here.
          (`(5 . ,_) nil)
          ;; Anything else is something to align.
          (_ (point))))
    (when (and bound (> (point) bound))
      (goto-char bound))))

(defun janet-align (beg end)
  "Vertically align the contents of the sexp around point.
If region is active, align it.  Otherwise, align everything in the
current \"top-level\" sexp.
When called from lisp code align everything between BEG and END."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (save-excursion
                   (let ((end (progn (end-of-defun)
                                     (point))))
                     (backward-sexp)
                     (list (point) end)))))
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (janet--find-sexp-to-align end)
      (let ((sexp-end (save-excursion
                        (backward-up-list)
                        (forward-sexp 1)
                        (point-marker)))
            (janet-align-forms-automatically nil)
            (count 1))
        ;; For some bizarre reason, we need to `align-region' once for each
        ;; group.
        (save-excursion
          (while (search-forward-regexp "^ *\n" sexp-end 'noerror)
            (cl-incf count)))
        (dotimes (_ count)
          (align-region (point) sexp-end nil
                        `((janet-align (regexp . janet--search-whitespace-after-next-sexp)
                                       (group . 1)
                                       (separate . ,janet-align-separator)
                                       (repeat . t)))
                        nil))
        ;; Reindent after aligning because of #360.
        (indent-region (point) sexp-end)))))


;;; Indentation
(defun janet-indent-region (beg end)
  "Like `indent-region', but also maybe align forms.
Forms between BEG and END are aligned according to
`janet-align-forms-automatically'."
  (prog1 (let ((indent-region-function nil))
           (indent-region beg end))
    (when janet-align-forms-automatically
      (condition-case nil
          (janet-align beg end)
        (scan-error nil)))))

(defun janet-indent-line ()
  "Indent current line as Janet code."
  (if (janet-in-docstring-p)
      (save-excursion
        (beginning-of-line)
        (when (and (looking-at "^\\s-*")
                   (<= (string-width (match-string-no-properties 0))
                       (string-width (janet-docstring-fill-prefix))))
          (replace-match (janet-docstring-fill-prefix))))
    (lisp-indent-line)))

(defvar janet-get-indent-function nil
  "Function to get the indent spec of a symbol.
This function should take one argument, the name of the symbol as
a string.  This name will be exactly as it appears in the buffer,
so it might start with a namespace alias.

This function is analogous to the `janet-indent-function'
symbol property, and its return value should match one of the
allowed values of this property.  See `janet-indent-function'
for more information.")

(defun janet--get-indent-method (function-name)
  "Return the indent spec for the symbol named FUNCTION-NAME.
FUNCTION-NAME is a string.  If it contains a `/', also try only
the part after the `/'.

Look for a spec using `janet-get-indent-function', then try the
`janet-indent-function' and `janet-backtracking-indent'
symbol properties."
  (or (when (functionp janet-get-indent-function)
        (funcall janet-get-indent-function function-name))
      (get (intern-soft function-name) 'janet-indent-function)
      (get (intern-soft function-name) 'janet-backtracking-indent)
      (when (string-match "/\\([^/]+\\)\\'" function-name)
        (or (get (intern-soft (match-string 1 function-name))
                 'janet-indent-function)
            (get (intern-soft (match-string 1 function-name))
                 'janet-backtracking-indent)))
      ;; indent symbols starting with if, when, ...
      ;; such as if-let, when-let, ...
      ;; like if, when, ...
      (when (string-match (rx string-start (or "if" "when" "let" "while") (syntax symbol))
                          function-name)
        (janet--get-indent-method (substring (match-string 0 function-name) 0 -1)))))

(defvar janet--current-backtracking-depth 0)

(defun janet--find-indent-spec-backtracking ()
  "Return the indent sexp that applies to the sexp at point.
Implementation function for `janet--find-indent-spec'."
  (when (and (>= janet-max-backtracking janet--current-backtracking-depth)
             (not (looking-at "^")))
    (let ((janet--current-backtracking-depth (1+ janet--current-backtracking-depth))
          (pos 0))
      ;; Count how far we are from the start of the sexp.
      (while (ignore-errors (backward-sexp 1)
                            (not (or (bobp)
                                     (eq (char-before) ?\n))))
        (cl-incf pos))
      (let* ((function (thing-at-point 'symbol))
             (method (or (when function ;; Is there a spec here?
                           (janet--get-indent-method function))
                         (ignore-errors
                           ;; Otherwise look higher up.
                           (pcase (syntax-ppss)
                             (`(,(pred (< 0)) ,start . ,_)
                              (goto-char start)
                              (janet--find-indent-spec-backtracking)))))))
        (when (numberp method)
          (setq method (list method)))
        (pcase method
          ((pred functionp)
           (when (= pos 0)
             method))
          ((pred sequencep)
           (pcase (length method)
             (`0 nil)
             (`1 (let ((head (elt method 0)))
                   (when (or (= pos 0) (sequencep head))
                     head)))
             (l (if (>= pos l)
                    (elt method (1- l))
                  (elt method pos)))))
          ((or `defun `:defn)
           (when (= pos 0)
             :defn))
          (_
           (message "Invalid indent spec for `%s': %s" function method)
           nil))))))

(defun janet--find-indent-spec ()
  "Return the indent spec that applies to current sexp.
If `janet-use-backtracking-indent' is non-nil, also do
backtracking up to a higher-level sexp in order to find the
spec."
  (if janet-use-backtracking-indent
      (save-excursion
        (janet--find-indent-spec-backtracking))
    (let ((function (thing-at-point 'symbol)))
      (janet--get-indent-method function))))

(defun janet--keyword-to-symbol (keyword)
  "Convert KEYWORD to symbol."
  (intern (substring (symbol-name keyword) 1)))

(defun janet--normal-indent (last-sexp indent-mode)
  "Return the normal indentation column for a sexp.
Point should be after the open paren of the _enclosing_ sexp, and
LAST-SEXP is the start of the previous sexp (immediately before
the sexp being indented).  INDENT-MODE is any of the values
accepted by `janet-indent-style'."
  (goto-char last-sexp)
  (forward-sexp 1)
  (backward-sexp 1)
  (let ((last-sexp-start nil))
    (if (ignore-errors
          ;; `backward-sexp' until we reach the start of a sexp that is the
          ;; first of its line (the start of the enclosing sexp).
          (while (string-match
                  "[^[:blank:]]"
                  (buffer-substring (line-beginning-position) (point)))
            (setq last-sexp-start (prog1 (point)
                                    (forward-sexp -1))))
          t)
        ;; Here we have found an arg before the arg we're indenting which is at
        ;; the start of a line. Every mode simply aligns on this case.
        (current-column)
      ;; Here we have reached the start of the enclosing sexp (point is now at
      ;; the function name), so the behaviour depends on INDENT-MODE and on
      ;; whether there's also an argument on this line (case A or B).
      (let ((indent-mode (if (keywordp indent-mode)
                             ;; needed for backwards compatibility
                             ;; as before janet-mode 5.10 indent-mode was a keyword
                             (janet--keyword-to-symbol indent-mode)
                           indent-mode))
            (case-a ; The meaning of case-a is explained in `janet-indent-style'.
             (and last-sexp-start
                  (< last-sexp-start (line-end-position)))))
        (cond
         ((eq indent-mode 'always-indent)
          (+ (current-column) lisp-body-indent -1))
         ;; There's an arg after the function name, so align with it.
         (case-a (goto-char last-sexp-start)
                 (current-column))
         ;; Not same line.
         ((eq indent-mode 'align-arguments)
          (+ (current-column) lisp-body-indent -1))
         ;; Finally, just align with the function name.
         (t (current-column)))))))

(defun janet--not-function-form-p ()
  "Non-nil if form at point doesn't represent a function call."
  (or (member (char-after) '(?\[ ?\{))
      (save-excursion ;; Catch #?@ (:cljs ...)
        (skip-chars-backward "\r\n[:blank:]")
        (when (eq (char-before) ?@)
          (forward-char -1))
        (and (eq (char-before) ?\?)
             (eq (char-before (1- (point))) ?\@)))
      ;; Car of form is not a symbol.
      (not (looking-at ".\\(?:\\sw\\|\\s_\\)"))))

;; Check the general context, and provide indentation for data structures and
;; special macros. If current form is a function (or non-special macro),
;; delegate indentation to `janet--normal-indent'.
(defun janet-indent-function (indent-point state)
  "When indenting a line within a function call, indent properly.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Janet function with a
non-nil property `janet-indent-function', that specifies how to do
the indentation.

The property value can be

- `defun', meaning indent `defun'-style;
- an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
- a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.
- a list, which is used by `janet-backtracking-indent'.

This function also returns nil meaning don't specify the indentation."
  ;; Goto to the open-paren.
  (goto-char (elt state 1))
  ;; Maps, sets, vectors and reader conditionals.
  (if (janet--not-function-form-p)
      (1+ (current-column))
    ;; Function or macro call.
    (forward-char 1)
    (let ((method (janet--find-indent-spec))
          (last-sexp calculate-lisp-indent-last-sexp)
          (containing-form-column (1- (current-column))))
      (pcase method
        ((or (pred integerp) `(,method))
         (let ((pos -1))
           (condition-case nil
               (while (and (<= (point) indent-point)
                           (not (eobp)))
                 (forward-sexp 1)
                 (cl-incf pos))
             ;; If indent-point is _after_ the last sexp in the
             ;; current sexp, we detect that by catching the
             ;; `scan-error'. In that case, we should return the
             ;; indentation as if there were an extra sexp at point.
             (scan-error (cl-incf pos)))
           (cond
            ;; The first non-special arg. Rigidly reduce indentation.
            ((= pos (1+ method))
             (+ lisp-body-indent containing-form-column))
            ;; Further non-special args, align with the arg above.
            ((> pos (1+ method))
             (janet--normal-indent last-sexp 'always-align))
            ;; Special arg. Rigidly indent with a large indentation.
            (t
             (+ (* 2 lisp-body-indent) containing-form-column)))))
        (`:defn
         (+ lisp-body-indent containing-form-column))
        ((pred functionp)
         (funcall method indent-point state))
        ;; No indent spec, do the default.
        (`nil
         (let ((function (thing-at-point 'symbol)))
           (cond
            ;; Preserve useful alignment of :require (and friends) in `ns' forms.
            ((and function (string-match "^:" function))
             (janet--normal-indent last-sexp 'always-align))
            ;; This should be identical to the :defn above.
            ((and function
                  (string-match "\\`\\(?:\\S +/\\)?\\(def[a-z]*\\|with-\\)"
                                function)
                  (not (string-match "\\`default" (match-string 1 function))))
             (+ lisp-body-indent containing-form-column))
            ;; Finally, nothing special here, just respect the user's
            ;; preference.
            (t (janet--normal-indent last-sexp janet-indent-style)))))))))

;;; Setting indentation
(defun put-janet-indent (sym indent)
  "Instruct `janet-indent-function' to indent the body of SYM by INDENT."
  (put sym 'janet-indent-function indent))

(defmacro define-janet-indent (&rest kvs)
  "Call `put-janet-indent' on a series, KVS."
  `(progn
     ,@(mapcar (lambda (x) `(put-janet-indent
                             (quote ,(car x)) ,(cadr x)))
               kvs)))

(defun add-custom-janet-indents (name value)
  "Allow `janet-defun-indents' to indent user-specified macros.

Requires the macro's NAME and a VALUE."
  (custom-set-default name value)
  (mapcar (lambda (x)
            (put-janet-indent x 'defun))
          value))

(defcustom janet-defun-indents nil
  "List of additional symbols with defun-style indentation in Janet.

You can use this to let Emacs indent your own macros the same way
that it indents built-in macros like with-open.  This variable
only works when set via the customize interface (`setq' won't
work).  To set it from Lisp code, use
     (put-janet-indent \\='some-symbol :defn)."
  :type '(repeat symbol)
  :set 'add-custom-janet-indents)

(define-janet-indent
  ;; built-ins
  (ns 1)
  (fn :defn)
  (def :defn)
  (def- :defn)
  (defn :defn)
  (defn- :defn)
  (bound-fn :defn)
  (if 1)
  (if-not 1)
  (case 1)
  (cond 0)
  (condp 2)
  (when 1)
  (while 1)
  (do 0)
  (comment 0)
  (doto 1)
  (fdef 1)

  (try 0)
  (catch 2)
  (finally 0)

  ;; binding forms
  (let 1)
  (loop 1)
  (each 1)
  (each 1)
  (for :defn)
  (doseq 1)
  (dotimes 1)
  (when-let 1)
  (if-let 1)
  (when-some 1)
  (if-some 1)

  (defmacro :defn)
  (defmacro- :defn))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.janet\\'" . janet-mode)))

(provide 'janet-mode)
;;; janet-mode.el ends here
