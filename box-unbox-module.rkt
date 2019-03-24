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
           ; plain-module-begin from LAL p5. p3
           ; Is "the base module wrapper" Adds no new semantics.
           ; Why does it run out of memory when I use plain-module-begin?
           ; https://docs.racket-lang.org/reference/module.html?q=local-expand#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._~23~25plain-module-begin%29%29
           [(module-begin forms ...)
            #:with core-forms (local-expand #`(#%plain-module-begin forms ...)
                                            'module-begin '())
            #:with boxed-unboxed (boxer-unboxer #'core-forms)
            #:with (_ form ...) #'boxed-unboxed
            #'(#%module-begin form ...); Why can't this be plain-module-begin?
            
            
            ]
           )
         ])
    (print result)
    result
    )
  )
    
    