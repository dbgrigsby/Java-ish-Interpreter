#lang racket

(provide (all-defined-out))
; declare: (cfuncs f g h i)
; call: (cfuncs-return cfuncstructval)
(struct cfuncs
  (
   return
   break
   continue
   catch
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

(define cfuncs-update-catch
  (lambda (cfuncsinstance newfunc)
    (struct-copy cfuncs cfuncsinstance [catch newfunc])))

(define cfuncs-wipe-all-but-catch
  (lambda (cfuncsinstance)
    (cfuncs-update-break (cfuncs-update-continue (cfuncs-update-return cfuncsinstance identity)
                                                 identity)
                         identity)))

(define identity-catch
  (lambda (a b)
    (error "called identity catch" a b)))
(define empty-cfuncs (cfuncs identity identity identity identity-catch))





