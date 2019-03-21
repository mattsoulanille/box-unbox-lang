#lang racket

(require (for-syntax syntax/parse racket/match racket/set racket/function))

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [box-unbox-module-begin #%module-begin]))

(define-syntax (box-unbox-module-begin stx)
  (syntax-parse stx
      [(module-begin expr ...) #`(#%module-begin expr ...)]
    )
  )
    
    