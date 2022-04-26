;;;; This library was ported from the mcas file in David McClain's Lisp-Actors library,
;;;; https://github.com/dbmcclain/Lisp-Actors/blob/main/data-objects/mcas-v4.lisp
;;;;
;;;; Licence
;;;; The vanilla Lisp-Actors is shared with licence "Unlicense License".
;;;; This licence claims that anyone is free to copy, modify, publish, use, compile,
;;;; sell, or distribute this software, either in source code form or as a compiled
;;;; binary, for any purpose, commercial or non-commercial, and by any means.
;;;;
;;;; Potability
;;;; The vanilla mcas can be used only in Lispworks,
;;;; this mcas library depends on Shinmera's atomics library,
;;;; which decides the potability of this library.
;;;; https://github.com/Shinmera/atomics
;;;;
;;;; Version
;;;; The version of this library will keep up with that of Lisp-Actors.


(defsystem "mcas"
  :version "1.0.0"
  :description "Multiple CAS on CAR/CDR of ref-cells. An N-way MCAS, without contention, needs only N+1 CAS instructions."
  :license "Unlicense"
  :depends-on (:atomics)
  :in-order-to ((test-op (test-op "mcas/test")))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "mcas")))))

(defsystem "mcas/test"
  :version "1.0.0"
  :license "Unlicense"
  :serial t
  :depends-on (:mcas
               :bordeaux-threads
               :parachute)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             )))
  :perform (test-op (o s) (uiop:symbol-call :parachute :test :mcas-test)))
