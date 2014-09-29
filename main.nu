; Remove both braces when deleting left brace in #{}
((ViMap insertMap) map:"<bs>" to:"<bs><del>" scope:"source.ruby.embedded.source.empty")

; Overwrite '}' in #{ .. }
((ViMap insertMap) map:"}" toExpression:(do ()
	(let (text (current-text))
		(if (eq '}' (text currentCharacter))
			(text input:"<right>")
			(else (text input:"<ctrl-v>}"))))) scope:"source.ruby string.quoted source.ruby.embedded")


;
; A set of line-oriented operations 
;

; returns the current carret location
(function caret ()
  ((current-text) caret))

; Returns the line number of the current line
(function current-line ()
  ((current-text) currentLine))

; Returns the line at the given caret location
(function line-at (location)
  (((current-text) textStorage) lineNumberAtLocation:location))

; Returns the text of the line in question
; nil if the line is out of bounds
(function line-string (line)
  (if (and (>= line (first-line)) (<= line (last-line)))
    (let (range (((current-text) textStorage) rangeOfLine:line))
  
      ((((current-text) textStorage) string) substringWithRange:range))))

; Returns the first line of the document
(function first-line ()
  1)

; Returns the last line. 0 if the document is empty
(function last-line ()
  (((current-text) textStorage) lineCount))
  
; Iterates through the lines in the given range and 
; Executes the lambda for each of those lines
; The lambda gets the line (number) as an argument
; 
; begin-line and end-line may be nil in which case the 
; first line and the last line of the current document are taken
(function each-line-in-line-range (begin-line end-line lambda)
  (let ((stop-line (cond 
                      ((not end-line)             (last-line))
                      ((> end-line (last-line))   (last-line))
                      (else                       end-line)))
        (start-line (cond
                      ((not begin-line)            (first-line))
                      ((< begin-line (first-line)) (first-line))
                      (else                        begin-line))))

    (for ((set l start-line) (<= l stop-line) (set l (+ l 1)))
      (lambda l))))

; Iterates through each line 
(function each-line (lambda)
  (each-line-in-line-range nil nil lambda))

; Iterates through each line after the line
(function each-line-from (line lambda)
  (each-line-in-line-range line nil lambda))

; Finds the next line matching the given pattern
(function line-matching-pattern (start-line pattern)
  (set result-line nil)

  (each-line-from start-line (do (line) 
    (set line-match (pattern findInString:(line-string line)))
    (if line-match
      (set result-line line)
      (break))))

  result-line)

(function line-matching-pattern-and-indent (start-line pattern indent)
  (set result-line nil)
  
  (each-line-from start-line (do (line)
    (if (== (indent-of-line line) indent)
      (set line-match (pattern findInString:(line-string line)))
      (if line-match
        (set result-line line)
        (break)))))
  result-line)

; Does the given line match the given pattern
(function line-matches-pattern? (line pattern)
  (pattern findInString:(line-string line)))

; Indent of the line in question
(function indent-of-line (line)
  (let (storage ((current-text) textStorage))
    (if (> line 0)
      (set lpos (storage locationForStartOfLine:line))
      (second (storage rangeOfLeadingWhitespaceForLineAtLocation:lpos))
      (else
        -1))))

; Inserts matching 'end' line whenever the user
; enters a ruby statement that marks the beginning of a block statement
; that requires and end.
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
  (let ((block-beginning (is-block-beginning?))
        (matching-end    (matching-end-line-exists? (+ 1 (current-line)))))
        
    (cond ((and block-beginning (not matching-end))  RUBY-CR-RUBY-CR-INSERT-END)
      ((block-beginning)                             RUBY-CR-INDENT-ONLY)
      (else                                          RUBY-CR-NORMAL))))

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
                 scope:"source.ruby")

;
; Utility functions
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

(set ruby-end-line-pattern /^\s* \b end \b/x)

(function matching-end-line-exists? (line)
  (set current-indent (indent-of-line (current-line)))
  (let ((first-end      (line-matching-pattern-and-indent line 
                                                          ruby-end-line-pattern
                                                          current-indent))
        (next-beginning (line-matching-pattern-and-indent line 
                                                          ruby-beginning-of-block-pattern
                                                          current-indent)))
      (cond
        ((and first-end next-beginning) (< first-end next-beginning))
        (first-end                      t)
        (else                           ()))))
  
(function is-block-beginning? ()
  (line-matches-pattern? (current-line) ruby-beginning-of-block-pattern))

