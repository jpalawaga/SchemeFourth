#lang scheme
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
   (printf (car input-list))
  )

