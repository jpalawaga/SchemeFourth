#lang scheme
(define first car)
(define rest cdr)

(define (read-keyboard-as-list)    ; this function returns keyboard input as a list
            (let ((char (read-char)))
              (if (char=? char #\newline)
                  '()
                  (let loop ((char char))
                     (if (char=? char #\newline)
                         '()
                         (cons char (loop (read-char)))
                     )
                  )
              )
            )
)

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

(define (main)
  (printf "UofL> ")
  (define raw-input (read-keyboard-as-string))
   (define input-list (string-split raw-input))
   (parse-input input-list)
   (main)
  )

(define parse-input (lambda (input)
  (cond
    (string->number (first input) (push_back (string->number (first input))))
    )
   (when (> (length (rest input)) 0)
       (parse-input (rest input))
       
     )
  ))

(define (push_back value)
  (printf "Pushing back: ")
  (displayln (number->string value))
  )