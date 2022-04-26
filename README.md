MCAS is a Common Lisp Multiple CAS library on CAR/CDR of ref-cells.
An N-way MCAS, without contention, needs only N+1 CAS instructions.

This library was ported from the mcas file in David McClain's Lisp-Actors library,
<https://github.com/dbmcclain/Lisp-Actors/blob/main/data-objects/mcas-v4.lisp>

# APIs
The APIs are quite simple, only expert mcas-ref, make-mcas-ref, mcas, and mcas-val.

## mcas-ref
A structure which is designed for mcas.

## make-mcas-ref (init-value)
Create an mcas-ref instance which can be mcased, the values will be compared by `eq`.

## mcas (&rest triples)
Return T if mcas succeeded, or NIl if it failed.
TRIPLES: a sequence of (ref old new) as would be suitable for CAS. But each ref must be a total-order MCAS-REF.

## mcas-val
Get the current value of an mcas-ref.

# Usage
The basis usage of mcas is very like that of normal cas, except mcas' arguments are triples.
`
(let* ((a  (make-mcas-ref 15))
       (b  (make-mcas-ref 16)))
  (mcas  a 15 32
         b 16 33)
  (list (mcas-val a) (mcas-val b)))
`

# Licence
The vanilla Lisp-Actors is shared with licence "Unlicense License".
This licence claims that anyone is free to copy, modify, publish, use, compile,
sell, or distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any means.

# Potability
The vanilla mcas can be used only in Lispworks,
this mcas library depends on Shinmera's atomics library,
which decides the potability of this library.
https://github.com/Shinmera/atomics

# Version
The version of this library will keep up with that of Lisp-Actors.
