#lang racket
(require (for-syntax syntax/parse
                     racket/match
                     racket/set
                     racket/function
                     "boxer-unboxer.rkt"))


(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [box-unbox-module-begin #%module-begin]))


;https://lexi-lambda.github.io/blog/2018/04/15/reimplementing-hackett-s-type-language-expanding-to-custom-core-forms-in-racket/
; We don't do a local-expand here since that means
; we wouldn't be able to parse any syntax that is
; custom to our language. It would only support racket stuff.
(define-syntax (box-unbox-module-begin stx)
  (let ([result
         (syntax-parse stx
           ; plain-module-begin from LAL p5
           [(module-begin expr ...)
            #:with core-forms (local-expand #`(#%plain-module-begin expr ...)
                                            'module-begin '())
            #'core-forms
            ]
           )
         ])
    (print result)
    result
    )
  )
    
    