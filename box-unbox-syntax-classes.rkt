#lang racket
(require syntax/parse)
(provide formals)

(define-syntax-class formals
  (pattern
   (id:identifier ...)
   #:attr args #'(id ...)
   #:attr rest #'())
  
  (pattern
   (id:identifier ...+ . rest-id:identifier)
   #:attr args #'(id ...)
   #:attr rest #'(rest-id))

  (pattern
   rest-id:identifier
   #:attr args #'()
   #:attr rest #'rest-id
   )
)
;(syntax-parse #'more
;  [a:formals #'a.args])

    
