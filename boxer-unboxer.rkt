#lang racket
(provide boxer-unboxer)
(require syntax/parse syntax/strip-context racket/syntax)
(require syntax/kerncase)


(require "box-unbox-syntax-classes.rkt")

;(require syntax/parse/lib/function-header)

(define (box-stx stx)
  #`(box #,stx))
(define (unbox-stx stx)
  #`(unbox #,stx))

            
(define (map-stx-list context-stx stx-procedure stx-list)
  (map stx-procedure
       (map (curry replace-context context-stx) stx-list)))

(define (box-unbox-list context-stx stx-list)
  (map-stx-list context-stx boxer-unboxer stx-list))

; TODO: Make sure it's okay to use the same context for each inner
; one as the outer one
(define (map-stx context-stx stx-procedure stx)
  (replace-context
   context-stx
   #`(#,@(map-stx-list context-stx stx-procedure (syntax->list stx)))))
  

(define (box-unbox-map-stx context-stx stx boxed-vars)
  (map-stx context-stx (lambda (stx) (boxer-unboxer-helper stx boxed-vars)) stx))

; Same as map but for multiple values instead of a list
(define (map-values stx)
  (syntax-parse stx
    [(func vals)
     #:with vals-lambda #'(lambda () vals)
     #:with res-list #'(call-with-values vals-lambda (lambda args (map func args)))
     #'(apply values res-list)]
    )
  )

; Checks if an identifier is bound to something in a list of identifiers.
(define (identifier-in-list ident id-list)
  (define (or-func a b) (or a b))
  (if (identifier? ident)
      (foldl or-func #f (map (lambda (other) (free-identifier=? ident other -1)) id-list))
      #f
  ))
  




; See https://docs.racket-lang.org/reference/syntax.html
; for a list of all racket/base syntactic forms.

; ~! means "if you get to here, don't consider any other possible forms
;           and fail if it doesn't end up matching"
(define (boxer-unboxer-helper stx boxed-vars)
  (define new-boxed-vars boxed-vars)
  (cons
   (syntax-parse stx
     ; define-values
     [((~literal define-values #:phase -1) ~! (var:identifier ...) vals:expr)
      ; Necessary so that functions that return multiple values get their values boxed properly.
      ; NOTE: values are not the same as arguments. You can't just use a function that takes multiple
      ; arguments on a `multiple value` multiple values are stored in the same argument. It's weird.
      #:do [(set! new-boxed-vars (append new-boxed-vars (syntax->list #'(var ...))))]
      #:with boxed-unboxed (let ([boxed-result (boxer-unboxer-helper #'vals new-boxed-vars)])
                             (set! new-boxed-vars (cdr boxed-result))
                             (car boxed-result)
                             )
      ;(print #'boxed-unboxed)
      
      (replace-context stx #`(define-values (var ...) #,(map-values #'(box boxed-unboxed))))]
     

     ; Function Calls
     ; If the function we're calling is one of our boxed functions, then
     ; make sure all of the arguments passed to it have been boxed.
     ; If it's a racket function, make sure all arguments passed to it are unboxed.
     ; Note that since we apply boxer-unboxer to each argument, they are all unboxed
     ; by the time the function is called.
     [((~literal #%app #:phase -1) ~! function arg ...)
      #:with (transformed-arg ...) (car (boxer-unboxer-helper #'(arg ...) new-boxed-vars))
      #:with (boxed-arg ...) #'((box transformed-arg) ...)
      #:with unboxed-func (car (boxer-unboxer-helper #'function new-boxed-vars))

      (if (identifier-in-list #'function new-boxed-vars)
          ; Then we are dealing with one of our functions,
          ; which needs all arguments boxed
          (replace-context stx #'(#%app unboxed-func boxed-arg ...))
          ; Else, we are dealing with a base racket function,
          ; which means we pass in unboxed values
          (replace-context stx #'(#%app unboxed-func transformed-arg ...))
          )]

     ; Function Declarations
     ; All variables given to the function are assumed to have been boxed by
     ; the Function Calls handler above.
     ; TODO: Make it work for rest
     [((~literal #%plain-lambda #:phase -1) ~! f:formals body ...+)
      #:with (transformed-body ...) (car (boxer-unboxer-helper
                                          #'(body ...)
                                          (append new-boxed-vars (syntax->list #'f.args))))
      (replace-context stx #'(#%plain-lambda f.args transformed-body ...))]

     [((~literal case-lambda #:phase -1) ~! [f:formals body ...+] ...)
      #:with ([transformed-body ...] ...)
      (map-stx stx (lambda (f-body)
                     (syntax-parse f-body
                       [(fname:formals body ...) 
                        (car (boxer-unboxer-helper
                              #'(body ...)
                              (append new-boxed-vars (syntax->list #'fname.args))))]
                        )
                     ) #'([f body ...] ...))
      (replace-context stx #'(case-lambda [f transformed-body ...] ...))]
      
     
     ; let-values
     [((~literal let-values #:phase -1) ~! ([(var:identifier ...) vals:expr] ...) body ...)
      #:do [(set! new-boxed-vars (append new-boxed-vars (syntax->list #'(var ... ...))))]
      #:with (transformed-vals ...) (car (boxer-unboxer-helper #'(vals ...) boxed-vars))
      ;(box-unbox-map-stx stx #'(vals ...) new-boxed-vars)
      #:with (boxed-vals ...) #`(#,@(map (lambda (v) (replace-context stx (map-values #`(box #,v))))
                                         (syntax->list #'(transformed-vals ...))))
      #:with (transformed-body ...) (car (boxer-unboxer-helper #'(body ...) new-boxed-vars))
      ;(box-unbox-map-stx stx #'(body ...) new-boxed-vars)
      ;(print #'(vals ...))
      ;(print #'(transformed-vals ...))
      (replace-context stx #'(let-values ([(var ...) boxed-vals] ...) transformed-body ...))]
     
     ; set!
     [((~literal set! #:phase -1) ~! var:identifier val:expr)
      #:with to-set-as (boxer-unboxer-helper #'val new-boxed-vars)
      (replace-context stx #'(set-box! var to-set-as))]
     
     [(any ...)
      #:with recursed #`(#,@(let ([stx-list (syntax->list #'(any ...))])
                              (for/list ([stx-item stx-list])
                                ;(print vars-list)
                                
                                (let* ([result (boxer-unboxer-helper stx-item new-boxed-vars)]
                                       [new-stx (car result)])
                                  (set! new-boxed-vars (cdr result))
                                  new-stx))))
      
      ;(map (lambda (x) (boxer-unboxer-helper x new-boxed-vars)) (syntax-e #`(any ...)))
      (replace-context stx #'recursed)]
     [single (if (identifier-in-list #'single new-boxed-vars)
                 (unbox-stx #'single)
                 #'single)]
     ) new-boxed-vars)
  )

(define (boxer-unboxer stx)
  (let ([result (boxer-unboxer-helper stx (list))])
    ;(print (cdr result))
    (car result)
    )
  )

        

; Questions:
; How do I detect variables?
; How do I detect if a variable is being defined vs being used?
; Why isn't bound-identifier=? working as I expect?
; Will I need a specific thing for every way a variable could be created, or
;   is there some better way to detect variable definition / binding?
; Would it be better to (expand stx) to racket/base before trying to
;   box / unbox all the variables?
; I can traverse the AST. What should the overall design structure of new Racets be?