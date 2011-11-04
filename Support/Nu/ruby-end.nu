; Inserts matching 'end' line whenever the user
; enters a ruby statement that marks the beginning of a block statement
; that requires an end.
;
; This brings Vico more in line with the behavior of opening quotes, 
; parenthesis; etc.


; Denotes that 'end' should be inserted and the next lin indented
(set RUBY-CR-RUBY-CR-INSERT-END 1)
; CR should indent but 'end' should not be inserted (matching end was found)
(set RUBY-CR-INDENT-ONLY 2)
; Normal CR handling
(set RUBY-CR-NORMAL 3)


; Determines the type of Ruby CR to process
(function type-of-cr ()
  (let ((blockb (is-block-beginning?))
        (matching-end (matching-end-exists-line? ((current-text) currentLine))))
        
    (cond ((and blockb (not matching-end))  RUBY-CR-RUBY-CR-INSERT-END)
      ((is-block-beginning?)                RUBY-CR-INDENT-ONLY)
      (else                                 RUBY-CR-NORMAL))))

; Normal CR
(function normal-cr-handling ()
  ((current-text) insertSnippet:"\n$0"))

; Start of block line was entered but a matching end was found
(function indent-only ()
  ((current-text) insertSnippet:"\n\t$0"))

; Start of block was entered and no matching end found
(function insert-end ()
  ((current-text) insertSnippet:"\n\t$0\nend"))
  
; CR Handler for ruby mode
(function handle-ruby-cr ()
  (case (type-of-cr)
    (RUBY-CR-RUBY-CR-INSERT-END   (insert-end))
    (RUBY-CR-INDENT-ONLY          (indent-only))
    (RUBY-CR-NORMAL               (normal-cr-handling))))

; Remap CR for insert mode for ruby files
((ViMap insertMap) map:"<cr>" 
          toExpression:(do () (handle-ruby-cr))
                 scope:"source.ruby.rails")

;
; Utility functions
;
(function range-of-content-after-line (line-nr)
  (let ((storage   ((current-text) textStorage))
        (next-line (+ line-nr 1)))
  
    (if (<= next-line (storage lineCount))
      (list (storage locationForStartOfLine:next-line)
            (- (storage length) (storage locationForStartOfLine:next-line))))))

(function indent-of (line)
  (let ((text     (current-text))
        (storage  ((current-text) textStorage)))

    (if (> line 0)
      (set lpos (storage locationForStartOfLine:line))
      (second (storage rangeOfLeadingWhitespaceForLineAtLocation:lpos))
      (else 
        -1))))

(function line-at (line-number)
  (let (range (((current-text) textStorage) rangeOfLine:line-number))
  
    ((((current-text) textStorage) string) substringWithRange:range)))

(set ruby-beginning-of-block-pattern /^
  ( \s*+
    (   module | class | def
      | unless | if
      | case
      | begin
      | for | while | until
      | (?= .*? \b(do|begin|case|if|unless)\b )
          (   "(\\.|[^\\"])*+"
            | '(\\.|[^\\'])*+'
            | [^\#"']
          )*
        (\s (do|begin|case)
          | [-+=&|*~%^<>~]  (?<!\$.) \s*+ (if|unless)))
    \b
    (?! [^;]*+ ; .*? \b end \b))
    .* $
  /x)

(function is-block-beginning? ()
  (let (line ((current-text) line))

    (ruby-beginning-of-block-pattern findInString:line)))


(set ruby-end-line-pattern /^\s* \b end \b/xm)

(function line-with-end-in-range (range)
  (set line-content ((((current-text) textStorage) string) substringWithRange:range))
  (set line-match (ruby-end-line-pattern findInString:line-content))

  (if line-match
    (((current-text) textStorage) lineNumberAtLocation:(+ 1 
                                                          (first range)
                                                          (first (line-match range))))))

(function matching-end-exists-line? (line-nr)
  (let (remaining-content (range-of-content-after-line line-nr))

    (if remaining-content
      (set matching-end (line-with-end-in-range remaining-content))
      (if matching-end
        (<= (indent-of line-nr) (indent-of matching-end))))))

