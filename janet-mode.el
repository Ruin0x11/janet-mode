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

;; Major mode for editing Janet. Mostly taken from `scheme-mode'.

;;; Code:

(require 'lisp-mode)

(defvar janet-mode-syntax-table
  (let ((st (make-syntax-table))
        (i 0))
    ;; Symbol constituents
    ;; We used to treat chars 128-256 as symbol-constituent, but they
    ;; should be valid word constituents (Bug#8843).  Note that valid
    ;; identifier characters are Janet-implementation dependent.
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    (modify-syntax-entry ?\| "\" 23bn" st)
    ;; Guile allows #! ... !# comments.
    ;; But SRFI-22 defines the comment as #!...\n instead.
    ;; Also Guile says that the !# should be on a line of its own.
    ;; It's too difficult to get it right, for too little benefit.
    ;; (modify-syntax-entry ?! "_ 2" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; It's used for single-line comments as well as for #;(...) sexp-comments.
    (modify-syntax-entry ?# "<"    st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    ;(modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar janet-mode-abbrev-table nil)
(define-abbrev-table 'janet-mode-abbrev-table ())

(defvar janet-imenu-generic-expression
      '((nil
         "^(\\(defn\\)*\\s-+(?\\(\\sw+\\)" 2)
        ;("Types"
        ; "^(define-class\\s-+(?\\(\\sw+\\)" 1)
        ("Macros"
         "^(\\(defmacro\\)\\s-+(?\\(\\sw+\\)" 2))
  "Imenu generic expression for Janet mode.  See `imenu-generic-expression'.")


(defun janet-mode-variables ()
  (set-syntax-table janet-mode-syntax-table)
  (setq local-abbrev-table janet-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local outline-regexp "### \\|(....")
  (setq-local add-log-current-defun-function #'lisp-current-defun-name)
  (setq-local comment-start "#")
  (setq-local comment-add 0)
  (setq-local comment-start-skip "#+[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local comment-column 40)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local lisp-indent-function 'lisp-indent-function)
  (setq mode-line-process '("" janet-mode-line-process))
  (setq-local imenu-case-fold-search t)
  (setq-local imenu-generic-expression janet-imenu-generic-expression)
  (setq-local imenu-syntax-alist '(("+-*/.<>=?!$%_&~^:" . "w")))
  (setq-local syntax-propertize-function #'janet-syntax-propertize)
  (setq font-lock-defaults
        '((janet-font-lock-keywords
           janet-font-lock-keywords-1 janet-font-lock-keywords-2)
          nil t (("+-*/.<>=!?$%_&~^:" . "w")
                                        ;(?#. "w 14")
                 )
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun)))
  (setq-local prettify-symbols-alist lisp-prettify-symbols-alist)
  (setq-local lisp-doc-string-elt-property 'janet-doc-string-elt)
  (setq-local inferior-lisp-program "janet")
  (setq-local inferior-lisp-prompt "^janet:\\([0-9]+\\):> *")
  (setq-local lisp-describe-sym-command "(doc %s)\n")
  (setq-local comint-use-prompt-regexp t))

(defvar janet-mode-line-process "")

(defvar janet-mode-map
  (let ((smap (make-sparse-keymap))
        (map (make-sparse-keymap "Janet")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar janet] (cons "Janet" map))
    (define-key map [run-janet] '("Run Inferior Janet" . run-janet))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)

    smap)
  "Keymap for Janet mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

(define-key janet-mode-map "\M-\C-x"  'lisp-eval-defun)     ; GNU convention
(define-key janet-mode-map "\C-x\C-e" 'lisp-eval-last-sexp) ; GNU convention
(define-key janet-mode-map "\C-c\C-e" 'lisp-eval-defun)
(define-key janet-mode-map "\C-c\C-r" 'lisp-eval-region)
(define-key janet-mode-map "\C-c\C-n" 'lisp-eval-form-and-next)
(define-key janet-mode-map "\C-c\C-p" 'lisp-eval-paragraph)
(define-key janet-mode-map "\C-c\C-c" 'lisp-compile-defun)
(define-key janet-mode-map "\C-c\C-z" 'switch-to-lisp)
; (define-key janet-mode-map "\C-c\C-l" 'lisp-load-file)
; (define-key janet-mode-map "\C-c\C-a" 'lisp-show-arglist)
(define-key janet-mode-map "\C-c\C-d" 'lisp-describe-sym)
(define-key janet-mode-map "\C-c\C-f" 'lisp-show-function-documentation)
(define-key janet-mode-map "\C-c\C-v" 'lisp-show-variable-documentation)

;;;###autoload
(define-derived-mode janet-mode prog-mode "Janet"
  "Major mode for editing Janet code.
Similar to `lisp-mode'."

  (janet-mode-variables))

(defconst janet-font-lock-keywords-1
  (eval-when-compile
    (list
     ;;
     ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
     ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
     (list (concat "(\\(define\\*?\\("
                   ;; Function names.
                   "\\(\\|-public\\|-method\\|-generic\\(-procedure\\)?\\)\\|"
                   ;; Macro names, as variable names.  A bit dubious, this.
                   "\\(-syntax\\|-macro\\)\\|"
                   ;; Class names.
                   "-class"
                   ;; Guile modules.
                   "\\|-module"
                   "\\)\\)\\>"
                   ;; Any whitespace and declared object.
                   ;; The "(*" is for curried definitions, e.g.,
                   ;;  (define ((sum a) b) (+ a b))
                   "[ \t]*(*"
                   "\\(\\sw+\\)?")
           '(1 font-lock-keyword-face)
           '(6 (cond ((match-beginning 3) font-lock-function-name-face)
                     ((match-beginning 5) font-lock-variable-name-face)
                     (t font-lock-type-face))
               nil t))
     ))
  "Subdued expressions to highlight in Janet modes.")

(defconst janet-font-lock-keywords-2
  (append janet-font-lock-keywords-1
   (eval-when-compile
     (list
      (cons
       (concat
        "(" (regexp-opt '("fn" "def" "cond" "loop" "try" "resume" "def-" "defn" "defmacro" "import" "with-syms" "when" "for" "if" "while" "do" "quote" "quasiquote" "and" "break") t) "\\>") 1)
      (cons
       (concat
        "\\<" (regexp-opt '("true" "false" "nil") t) "\\>")
        '(1 font-lock-constant-face))
      (cons
       (concat
        "\\<" (regexp-opt '("error") t) "\\>")
        '(1 font-lock-warning-face))
      (cons
       (concat
        "(" (regexp-opt '("print" "printf" "doc") t) "\\>")
        '(1 font-lock-builtin-face))
      '("\\<\\(\\sw+\\)/\\(\\sw+\\)\\>"
        (1 font-lock-type-face))
      '("(\\(defn\\|defmacro\\|fn\\)\\s-+\\(\\sw+\\)"
        (2 font-lock-function-name-face))
      '("(\\(def\\|var\\)\\s-+\\(\\sw+\\)"
        (2 font-lock-variable-name-face))
      '("\\<:\\sw+\\>" . font-lock-constant-face)
      '("\\(@\\)[\\[\\|{]" . (1 font-lock-preprocessor-face))
      )))
  "Gaudy expressions to highlight in Janet modes.")

(defvar janet-font-lock-keywords janet-font-lock-keywords-1
  "Default expressions to highlight in Janet modes.")

(defconst janet-sexp-comment-syntax-table
  (let ((st (make-syntax-table janet-mode-syntax-table)))
    (modify-syntax-entry ?# "." st)
    (modify-syntax-entry ?\n " " st)
    ;(modify-syntax-entry ?#  "'" st)
    st))

(put 'lambda 'janet-doc-string-elt 2)
;; Docstring's pos in a `define' depends on whether it's a var or fun def.
(put 'define 'janet-doc-string-elt
     (lambda ()
       ;; The function is called with point right after "define".
       (forward-comment (point-max))
       (if (eq (char-after) ?\() 2 0)))

(defun janet-syntax-propertize (beg end)
  (goto-char beg)
  (janet-syntax-propertize-sexp-comment (point) end)
  (funcall
   (syntax-propertize-rules
    ("\\(#\\);" (1 (prog1 "< cn"
                     (janet-syntax-propertize-sexp-comment (point) end)))))
   (point) end))

(defun janet-syntax-propertize-sexp-comment (_ end)
  (let ((state (syntax-ppss)))
    (when (eq 2 (nth 7 state))
      ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
      (condition-case nil
          (progn
            (goto-char (+ 2 (nth 8 state)))
            ;; FIXME: this doesn't handle the case where the sexp
            ;; itself contains a #; comment.
            (forward-sexp 1)
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "> cn")))
        (scan-error (goto-char end))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.janet\\'" . janet-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("janet" . janet-mode))

(provide 'janet-mode)
;;; janet-mode.el ends here
