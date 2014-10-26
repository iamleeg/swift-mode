;;; swift-mode.el --- Major mode for editing Swift files

(defgroup swift nil
  "Major mode for editing Swift code."
  :prefix "swift-"
  :group 'languages)

(defcustom swift-comment-column 32
  "Indentation column of comments."
  :type 'integer :group 'swift)

;; Safe file variables
(put 'swift-comment-column 'safe-local-variable 'integerp)

(setq swift-keywords-declarations
      '("class" "enum" "extension" "func" "import" "init" "internal" "let" "operator" "private" "protocol" "public" "static" "struct" "subscript" "typealias" "var"))

(setq swift-keywords-statements
      '("break" "case" "continue" "default" "do" "else" "fallthrough" "for" "if" "in" "return" "switch" "where" "while"))

(setq swift-keywords-expressions
      '("as" "dynamicType" "false" "is" "nil" "self" "Self" "super" "true" "__COLUMN__" "__FILE__" "__FUNCTION__" "__LINE__"))

(setq swift-keywords-context
      '("associativity" "convenience" "dynamic" "didSet" "final" "get" "infix" "inout" "lazy" "left" "mutating" "none" "nonmutating" "optional" "override" "postfix" "precedence" "prefix" "Protocol" "required" "right" "set" "Type" "unowned" "weak" "willSet"))

(setq swift-keywords
      (append swift-keywords-declarations
              swift-keywords-statements
              swift-keywords-expressions
              swift-keywords-context))

(setq swift-builtins
      '("println"))

(setq swift-keywords-regexp (regexp-opt swift-keywords 'words))
(setq swift-constants-regexp
      '("\\b[0-9]+\\b" "\\b0x[0-9a-fA-F]+\\b" "\\btrue\\b" "\\bfalse\\b"))
(setq swift-strings-regexp '("\"[^\"]*\""))
(setq swift-builtins-regexp (regexp-opt swift-builtins 'words))

(setq swift-font-lock-keywords `((,swift-keywords-regexp
                                  . font-lock-keyword-face)
                                 (,swift-constants-regexp
                                  . font-lock-constant-face)
                                 (,swift-strings-regexp . font-lock-string-face)
                                 (,swift-builtins-regexp . font-lock-builtin-face)))

(defvar swift-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in Swift mode.")

(defvar swift-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for Swift files.")

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.swift\\'") 'swift-mode))

;;;###autoload
(define-derived-mode swift-mode prog-mode "Swift"
  "Major mode for editing Swift files.

\\{swift-mode-map}"
  (set (make-local-variable 'font-lock-defaults) '((swift-font-lock-keywords)))
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end)   " */")
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")
  (set (make-local-variable 'comment-column) swift-comment-column)
  (set (make-local-variable 'comment-use-syntax) t))

(provide 'swift-mode)
