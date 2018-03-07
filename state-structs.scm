#lang racket

(provide (all-defined-out))
; declare: (cfuncs f g h i)
; call: (cfuncs-return cfuncstructval)
(struct cfuncs
  (
   return
   break
   continue
   throw
   ))

(define cfuncs-update-return
  (lambda (cfuncsinstance newfunc)
    (struct-copy cfuncs cfuncsinstance [return newfunc])))

(define cfuncs-update-break
  (lambda (cfuncsinstance newfunc)
    (struct-copy cfuncs cfuncsinstance [break newfunc])))

(define cfuncs-update-continue
  (lambda (cfuncsinstance newfunc)
    (struct-copy cfuncs cfuncsinstance [continue newfunc])))

(define cfuncs-update-throw
  (lambda (cfuncsinstance newfunc)
    (struct-copy cfuncs cfuncsinstance [throw newfunc])))