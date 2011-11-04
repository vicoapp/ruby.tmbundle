; Some basic operations wih lines

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
                      ((not end-line) (last-line))
                      ((> end-line)   (last-line))
                      (else           end-line)))
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
