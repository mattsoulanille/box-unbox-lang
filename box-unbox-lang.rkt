#lang racket/base

(module reader syntax/module-reader box-unbox-lang/box-unbox-module
  #:read-syntax
  (lambda (name in) (read-syntax name in))
  #:read
  (lambda (in)
    (map syntax->datum (read-syntax 'prog in)))
  
  #:whole-body-readers?
  #t
  (require racket)

  )
           