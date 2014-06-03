;;; swift-mode.el --- Major mode for editing Swift files

(setq swift-keywords
      '("break" "class" "continue" "default" "do" "else" "for" "func" "if" "import" "in" "let" "return" "self" "struct" "super" "switch" "unowned" "var" "weak" "while"))

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
    table)
  "Syntax table for Swift files.")

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.swift\\'") 'swift-mode))

;;;###autoload
(define-derived-mode swift-mode prog-mode "Swift"
   "Major mode for editing Swift files.

\\{swift-mode-map}"
   (setq font-lock-defaults '((swift-font-lock-keywords))))

(provide 'swift-mode)
