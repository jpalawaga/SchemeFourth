(define first car)
(define rest cdr)
(define stack '())
(define tempArea '())
(define currentCommand '())
(define curFunc '())
(define funcList (make-hash-table 'equal ) )
(define funcDefStarted #f)
(define funcNames '() )
(define grammarStack '())
(define ifCount 0)
(define ifQueue '())
(define ignore #f)
(define lastConditional '())
(define quoteFlag #f)
(define squishQueue '())
(define  messageQueue '())

(define (read-keyboard-as-string) ; this function returns keyboard input as a string
            (let ((char (read-char)))
              (if (char=? char #\newline)
                  '()
                  (list->string
                   (let loop ((char char))
                     (if (char=? char #\newline)
                         '()
                         (cons char (loop (read-char)))
                     )
                   )
                  )
              )
            )
)

(define (string-join x)
  (cond 
    ((empty? (rest x))
     (first x))
    (else (string-append (first x) " " (string-join (rest x))))))

(define (stringSquish x)
  (cond
    ((not quoteFlag)
     (cond
       ((equal? "\"" (substring (first x) 0 1)) (begin
                                          (set! quoteFlag #t)
                                          (set! messageQueue (append messageQueue (list (first x))))
                                          (if (not (empty? (rest x)))
                                              (stringSquish (rest x))
                                              squishQueue)))
       (else (begin
               (set! squishQueue (append squishQueue (list (first x))))
               (if (not (empty? (rest x)))
                   (stringSquish (rest x))
                   squishQueue)))))
    (else (cond
            ((equal? "\"" (substring (first x) (- (string-length (first x)) 1) (string-length (first x)))) (begin
                                                                                                      (set! quoteFlag #f)
                                                                                                      (set! messageQueue (append messageQueue (list (first x))))
                                                                                                      (set! squishQueue (append squishQueue (list (string-join messageQueue))))
                                                                                                      (set! messageQueue '())
                                                                                                      (if (not (empty? (rest x)))
                                                                                                          (stringSquish (rest x))
                                                                                                          squishQueue)))
            (else (begin
                    (set! messageQueue (append messageQueue (list (first x))))
                    (if (not (empty? (rest x)))
                        (stringSquish (rest x))
                        squishQueue)))))))
       

(define (push_grammar element)
  (set! grammarStack (append (list element) grammarStack) 
       )
  )
  
(define (push_back element) 
  (set! stack (append (list element) stack) 
       )
  )

(define (DROP) (begin 
                 (define temp '() )
                 (set! stack 
                       (append temp (cdr stack) ) 
                       )
                 )
  )

(define (POP)
  (define x (first stack))
  (set! stack (rest stack))
  x
  )

(define (SAVE_POP)
  (set! tempArea (first stack))
  (set! stack (rest stack))
  tempArea
  )

(define (SAVE)
  (push_back tempArea)
  )

(define (DUP)
  (push_back (first stack))
  )

(define (SWAP)
  (define x (first stack))x
  (SAVE_POP)
  (SAVE_POP)
  (push_back x)
  (SAVE)
  )

(define (CLEAR)
  (set! stack '())
  )

(define (STACK x)
  (cond ((not (empty? x))
     (display (first x))
     (display #\newline)
     (STACK (rest x)))
  ))

(define (IF x tv)
  (cond
    ((not tv) (cond
                 ((string-ci=? "IF" (first x)) (begin
                                              (set! ifCount (+ ifCount 1))
                                              (IF (rest x) #f)))
                 ((string-ci=? "THEN" (first x)) (begin
                                                (set! ifCount (- ifCount 1))
                                                (IF (rest x) #f)))
                 ((string-ci=? "ELSE" (first x)) (cond 
                                                   ((= 0 ifCount) (IF (rest x) #t))
                                                   (else (IF (rest x) #f))))
                 (else (IF (rest x) #f))))
    (else (begin
            (if (not ignore) (set! ifQueue (append ifQueue (list (first x)))))
            (cond
              ((string-ci=? "IF" (first x)) (begin
                                           (set! ifCount (+ ifCount 1))
                                           (IF (rest x) #t)))
              ((string-ci=? "THEN" (first x)) (cond
                                             ((= 0 ifCount) (begin
                                                              (if (not ignore)
                                                                  (set! ifQueue (reverse (cdr (reverse ifQueue)))))
                                                              (parse-input ifQueue)
                                                              (if (not (empty? (rest x)))
                                                                  (parse-input (rest x)))))
                                             (else (begin
                                                     (set! ifCount (- ifCount 1))
                                                     (IF (rest x) #t)))))
              ((string-ci=? "ELSE" (first x)) (cond 
                                             ((= 0 ifCount) (begin
                                                              (set! ignore #t)
                                                              (set! ifQueue (reverse (cdr (reverse ifQueue))))
                                                              (IF (rest x) #t)))
                                             (else
                                              (IF (rest x) #t))))
              (else (IF (rest x) #t)))))))

(define (LOOP conditional body afterInstructions)
  ;if true
    ;do body
    ;evaluate condition
    ;call loop.
  (if (first stack)
      (begin
       (parse-input body)
       (parse-input conditional)
       (LOOP conditional body afterInstructions)
       ; Execute remainder of instructions
        )
      ( begin
               (if (not (null? afterInstructions))
                   (parse-input afterInstructions)))
    )
 )

; Returns a list of instructions that happen after a loop is closed.
(define (getAfterInstructions input)
  (define reversed (reverse input))
  (define getInstructions (lambda (x)
     (if (not(string=? (first x) "POOL"))
         (append (getInstructions (rest x)) (list (first x)))
         '()
         )
  ))
  
  (getInstructions reversed)
)

(define (getLoopBody input loopLevel)
  ; Nothing left to do
    (if (= 0 (length input))
      '()
      (begin
  
  ; Change based on condition
   (cond
    ((string-ci=? "loop" (first input)) (set! loopLevel (+ 1 loopLevel)))
    ((string-ci=? "pool" (first input)) (set! loopLevel (- loopLevel 1)))
    )

  ;
  (if (> loopLevel 0)
      (cond
        ((and (= loopLevel 1) (string=? (first input) "LOOP"))
           (getLoopBody (rest input) loopLevel))
      (else
          (append (list (first input)) (getLoopBody (rest input) loopLevel)))
      )
      '())
 )))


(define (FUNC funcCommands)
  (define tempList '() )
  (if (equal? funcDefStarted #f) ;check if the definition has not been started yet
      ;create a new hash with the function name as the first element of commands list with the result of the commands until "CNUF" 
      (begin ; definition not started yet
       (set! funcDefStarted #t) ; start definition
       (set! funcNames (append funcNames (list (first funcCommands)) ) )
       (hash-table-put! funcList (first funcCommands) (FUNC (rest funcCommands) ) ) 
       (set! funcDefStarted #f)
       ; display resulting function 
       ;(display (hash-table-get funcList (first funcCommands) ) )
       (display funcNames)
       (display #\newline)
       
        
       ) 
      
      ;definition has been started 
      (if (string? (first funcCommands) ) ;checks to see if first element is a string
          ; it is a string
          (if (string-ci=? (first funcCommands) "CNUF" ) ;is the string "CNUF" 
              ; it is "CNUF", return empty list
              (if (not (= (length (rest funcCommands)) 0) ) ;is the rest of the command list empty 
                  (begin ; it is not empty, do commands and return an empty list
                    (parse-input (rest funcCommands))
                    '()
                    )
                   ; it is empty, just return empty list
                   '()
                  )
              
              ; it isn't "CNUF", return result of rest of command list by appending first element and the result of calling FUNC with the rest of the list
              (append (list (first funcCommands) ) (FUNC (rest funcCommands) ) )
              ; should return the resulting list after both appends  
              )
          ; it isn't a string 
           (append (list (first funcCommands)) (FUNC (rest funcCommands) ) )
          
        ) 
      ); end of function definition if
  
  )

(define (callUserFunc funcCall) 
  (parse-input (hash-table-get funcList funcCall) )
  
  )

(define (checkExistingFuncs name list) 
  (define exists #f)
  (if (> (length list) 0) ;if we haven't hit the end of the list
      (if (string=? name (first list) ) ;check to see if the names are equal 
          (set! exists #t) ;they are equal, set exists to true 
          (set! exists (checkExistingFuncs name (rest list) )) ;they still are not equal, make a recursive call with he rest of the list and set exists to the result  
           )
      exists ;we hit the end of the list, base case, return exists 
      )
  exists ;return the result 
  )

(define (main)
  (display "UofL> ")
  (set! quoteFlag #f)
  (set! messageQueue '())
  (set! squishQueue '())
  (let 
   ((input-list (regexp-split #px" " (read-keyboard-as-string))))
    (set! input-list (stringSquish input-list))
    (set! currentCommand '())
    (set! grammarStack '())
    (if (grammar-check input-list)
        (parse-input currentCommand)
        (begin
          (display "Grammatical Error")
          (display #\newline)))
    (main)
    )
  )

(define grammar-check (lambda (input)
  (define errFree #t)
  (define return #t) 
  (if (string? (first input))
      (begin  
        (set! currentCommand (append currentCommand (list (first input))))
        (cond
          ((string-ci=? "IF" (first input)) (push_grammar(first input)))
          ((string-ci=? "LOOP" (first input)) (push_grammar(first input)))
          ((string-ci=? "ELSE" (first input)) (if (string-ci=? "IF" (first grammarStack)) (set-car! grammarStack (first input)) (set! errFree #f))) 
          ((string-ci=? "THEN" (first input)) (if (string-ci=? "ELSE" (first grammarStack)) (set! grammarStack (rest grammarStack)) (set! errFree #f)))
          ((string-ci=? "POOL" (first input)) (if (string-ci=? "LOOP" (first grammarStack)) (set! grammarStack (rest grammarStack)) (set! errFree #f))))))
                        
  (if errFree
      (begin
        (if (and (empty? grammarStack) (empty? (rest input)))
            (set! return #t)
            (begin
              (if (not (empty? (rest input)))
                  (begin
                    (if(grammar-check(rest input))
                       (set! return #t)
                       (set! return #f)))
                  (begin
                    (set! quoteFlag #f)
                    (set! messageQueue '())
                    (set! squishQueue '())
                    (if (grammar-check(stringSquish(regexp-split #px" " (read-keyboard-as-string))))
                        (set! return #t)
                        (set! return #f)))))))
      (set! return #f))
   return                 
))

(define parse-input (lambda (input)
  (cond
    ((string-ci=? "" (first input)) )
    ((string-ci=? "\t" (first input)) )
    ((string->number (first input)) (push_back (string->number (first input))))
    ((string=? "+" (first input)) (push_back (+ (POP) (POP))))
    ((string=? "-" (first input)) (push_back (- (POP) (POP))))
    ((string=? "/" (first input)) (push_back (/ (POP) (POP))))
    ((string=? "*" (first input)) (push_back (* (POP) (POP))))
    ((string=? ">" (first input)) (let() (define x (POP))
                                         (push_back (> (POP) x))
                                         (set! lastConditional (append (list (number->string x)) (list ">")))
                                         ))
         ((string=? "<" (first input)) (let() (define x (POP))
                                         (push_back (< (POP) x))
                                         (set! lastConditional (append (list (number->string x)) (list "<")))
                                         
                                         ))
         ((string=? "<=" (first input)) (let()(define x (POP))
                                          (push_back (<= (POP) x))
                                          (set! lastConditional '()) 
                                          (set! lastConditional (append (list (number->string x)) (list "<=")))
                                          ))
         
         ((string=? ">=" (first input)) (let()(define x (POP))
                                          (push_back (>= (POP) x))
                                          (set! lastConditional '())
                                          (set! lastConditional (append (list (number->string x)) (list ">=")))
                                          ))
    ((string-ci=? "SWAP" (first input)) (SWAP))
    ((string-ci=? "DUP" (first input)) (DUP))
    ((string-ci=? "POP" (first input)) (SAVE_POP))
    ((string-ci=? "SAVE" (first input)) (SAVE))
    ((string-ci=? "DROP" (first input)) (DROP))
    ((string-ci=? "CLEAR" (first input)) (CLEAR))
    ((string-ci=? "STACK" (first input)) (STACK stack))
    ((string-ci=? "IF" (first input)) (begin
                                     (set! ifCount 0)
                                     (set! ifQueue '())
                                     (set! ignore #f)
                                     (IF (rest input) (first stack))))
    ((string-ci=? "LOOP" (first input)) (if (first stack) (LOOP lastConditional (getLoopBody input 0) (getAfterInstructions (rest input))) () ))
    ((string-ci=? "FUNC" (first input)) (FUNC (rest input)) )
    ((string-ci=? "." (first input)) 
     (cond
       ((not (empty? (rest input))) (begin
                                      (display (cadr input))
                                      (display #\newline)
                                      (if (not (empty? (cddr input)))
                                          (parse-input (cddr input)))))
       (else (first STACK))))
    (else (if (checkExistingFuncs (first input) funcNames) 
              (callUserFunc (first input) ) 
              (begin 
                (display "Function name does not exist")
                (display #\newline)
                )
              )
          )
    )
                      
                      
                      ;borat when clause. 'nawt nawt nawt'
                      ;THIS STATEMENT EXECUTES NOT
   (when (and (and (and (and (> (length (rest input)) 0) (not (string-ci=? "loop" (first input)))) (not (string-ci=? "func" (first input)))) (not (string-ci=? "if" (first input)))) (not (string-ci=? "." (first input))))
       (parse-input (rest input))
         
  ))
  

(define (string->stringList input)
  (apply string-append ( rest ( apply append (map (lambda (x) (list " " x)) (rest input)))))
)

(main)
