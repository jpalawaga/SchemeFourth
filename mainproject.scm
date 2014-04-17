(define first car)
(define rest cdr)
(define stack '())
(define tempArea '())
(define currentCommand '())
(define curFunc '())
(define funcList (make-hash-table) )
(define funcDefStarted #f)

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
  (define x (first stack))
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


; truth condition is already on top of the stack
(define (LOOP x)
  ; Assuming that "LOOP" is parsed
  ; if true (?)
  ;   If pool
  ;      parse-input currentCommand
  ;   If not
  ;      execute first x
  (cond
    ((string-ci=? "pool" (first x)) (parse-input currentCommand))
    (else 
     (begin
      (parse-input (list (first x)))
      (LOOP (rest x)))
     )
   )
  )

(define (FUNC funcCommands)
  (define tempList '() )
  (if (equal? funcDefStarted #f) ;check if the definition has not been started yet
      ;create a new hash with the function name as the first element of commands list with the result of the commands until "CNUF" 
      (begin ; definition not started yet
       (set! funcDefStarted #t) ; start definition
       (hash-table-put! funcList (first funcCommands) (FUNC (rest funcCommands) ) ) 
       (set! funcDefStarted #f)
       ; display resulting function 
       (display (hash-table-get funcList (first funcCommands) ) )
       (display #\newline)
       
        
       ) 
      
      ;definition has been started 
      (if (string? (first funcCommands) ) ;checks to see if first element is a string
          ; it is a string
          (if (string-ci=? (first funcCommands) "CNUF" ) ;is the string "CNUF" 
              ; it is "CNUF", return empty list
              (begin 
                (parse-input (rest funcCommands))
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
  
  

(define (main)
  (display "UofL> ")
  (let
      ((input-list (regexp-split #px" " (read-keyboard-as-string))))
    (set! currentCommand input-list)
    (parse-input input-list)
    (main)
    )
  )

(define parse-input (lambda (input)
  (cond
    ((string->number (first input)) (push_back (string->number (first input))))
    ((string=? "+" (first input)) (push_back (+ (POP) (POP))))
    ((string=? "-" (first input)) (push_back (- (POP) (POP))))
    ((string=? "/" (first input)) (push_back (/ (POP) (POP))))
    ((string=? "*" (first input)) (push_back (* (POP) (POP))))
    ((string=? ">" (first input)) (push_back (> (POP) (POP))))
    ((string=? "<" (first input)) (push_back (< (POP) (POP))))
    ((string=? ">=" (first input)) (push_back (>= (POP) (POP))))
    ((string=? "<=" (first input)) (begin (SWAP) (push_back (<= (POP) (POP)))))
    ((string-ci=? "swap" (first input)) (SWAP))
    ((string-ci=? "dup" (first input)) (DUP))
    ((string-ci=? "pop" (first input)) (SAVE_POP))
    ((string-ci=? "save" (first input)) (SAVE))
    ((string-ci=? "drop" (first input)) (DROP))
    ((string-ci=? "clear" (first input)) (CLEAR))
    ((string-ci=? "stack" (first input)) (STACK stack))
    ((string-ci=? "loop" (first input)) (if (first stack) (LOOP (rest input) ) () ))
    ((string-ci=? "func" (first input)) (FUNC (rest input)) )
    ((string-ci=? "." (first input)) 
     ( let ()
        (if (> (length input) 1)
            (display (string->stringList input))
            (display (first stack)))
        (display #\newline)))
    )
                      
                      ;borat when clause. 'nawt nawt nawt'
                      ;THIS STATEMENT EXECUTES NOT
   (when (and (and (> (length (rest input)) 0) (not (string-ci=? "loop" (first input)))) (not (string-ci=? "func" (first input))))
       (parse-input (rest input))
       
     )
  ))
  

(define (string->stringList input)
  (apply string-append ( rest ( apply append (map (lambda (x) (list " " x)) (rest input)))))
)

(main)