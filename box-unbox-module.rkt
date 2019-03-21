#lang racket
(require (for-syntax syntax/parse
                     racket/match
                     racket/set
                     racket/function
                     "boxer-unboxer.rkt"))


(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [box-unbox-module-begin #%module-begin]))

(define-syntax (box-unbox-module-begin stx)
  (let ([result
         (syntax-parse stx
           [(module-begin expr ...) #`(#%module-begin #,@(boxer-unboxer #`(expr ...)))]
           )
         ])
    (print result)
    result
    )
  )
    
    