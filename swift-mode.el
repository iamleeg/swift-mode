;;; swift-mode.el --- Major mode for editing Swift files

(setq swift-keywords
      '("break" "continue" "default" "do" "else" "for" "func" "if" "in" "let" "return" "self" "struct" "super" "switch" "unowned" "var" "weak" "while"))

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

(define-derived-mode swift-mode fundamental-mode "Swift"
   "Major mode for editing Swift files."
   (setq font-lock-defaults '((swift-font-lock-keywords))))

(provide 'swift-mode)
