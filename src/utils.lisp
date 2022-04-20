;;;; This file contains the utils used by mcas,
;;;; and they are originated from Lisp-Actors/useful-macros,
;;;; with necessary compatility modification.

(in-package :mcas)

;; parse-body.lisp
(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  ;; this function can be found in alexandria
  ;; if documentation is true, parse the string item in body as a docstring, mostly one string allowed.
  ;; returns: (values function-body-form function-declare-form function-docstring)
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (push (pop body) doc)) ;; Alexandria fails here...
         (go :declarations))
       (when (and (listp current) (eql (first current) 'declare))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))

;; xlambda.lisp
(defun is-underscore? (x)
  (and (symbolp x)
       (string= "_" (string x))))

;; xlambda.lisp
(defun decl-us (args) ; declare underscore
  "If args is an underscore symbol, return list ((DECLARE (IGNORE _))) to ignore this arg."
  (when (is-underscore? args)
    `((declare (ignore ,args)))))

;; xlambda.lisp
(defun us-conv (args)
  (cond ((eq nil args)  nil)
        ((symbolp args) `(&rest ,args))
        (t              args)))

(defun dotted-list-p (lst)
  "LW doc: The function dotted-list-p is a predicate which tests whether
list (which must be a cons) is a list ending in a non-nil cdr.
It returns true if this is the case, otherwise it returns false."
  ;; in lw (lwpe7.1 win64), calling dotted-list-p with any non-list object will return the object itself.
  (if (listp lst)
      (cdr (last lst))
      lst))

;; xlambda.lisp
(defun is-lambda-list-keyword (arg)
  "Test if ARG is a legal lambda list keyword such as &key, &optional, etc."
  (member arg lambda-list-keywords))

;; xlambda.lisp
(defun destr-lambda-list-p (args)
  "Test if a lambda list can be destructured or not."
  (and (consp args)
       (or (eq (car args) '&whole)
           ;;(lw:dotted-list-p args)
           (dotted-list-p args)
           (some 'consp (subseq args 0
                                (position-if #'is-lambda-list-keyword args))))))

;; xlambda.lisp
(defun wrap-assembly (name args &rest body)
  "Eg. if NAME is lambda, this function will return a form like (lambda (&rest gensym) new-body),
the NEW-BODY is a destructuring-bind form which destructures a gensym into ARGS and wraps BODY."
  ;; this functions a form, and it's purpose is to wrap some definition at some place.
  (if (destr-lambda-list-p args) ; test if args can be destructured or not
      (let ((g!args (gensym)))
        (multiple-value-bind (body-forms decls docstr)
            (parse-body body :documentation t)
          `(,name (&rest ,g!args)
                  ,@docstr
                  (destructuring-bind ,args ,g!args
                    ,@decls
                    ,@body-forms))
          ))
      ;; else
      `(,name ,(us-conv args) ,@(decl-us args) ,@body)))

;; xlambda.lisp
(defmacro lambda* (args &body body)
  "Return an anonymous function definition, (lambda (&rest gensym) new-body)
which destructures a gensym and wrap BODY in a destructuring-bind form."
  ;; this macro's purpose is to wrap a lambda definition at some place.
  (apply #'wrap-assembly 'lambda args body))

;; useful-macros.lisp
(defmethod group ((seq sequence) n &key from-end)
  "Partition SEQ and return a list of SEQUENCE type objects with length N,
the original order will be kept."
  ;; (group #(1 2 3 4 5 6 7 8 9 0) 3) -> '(#(1 2 3) #(4 5 6) #(7 8 9) #(0))
  (check-type n (integer 1))
  (let ((stop (length seq)))
    (labels ((rec (start end acc)
               (if (>= end stop)
                   (nreverse (cons (subseq seq start stop) acc))
                   (rec end (+ end n) (cons (subseq seq start end) acc)))))
      (when (plusp stop)
        (cond (from-end
               (let ((start (mod stop n)))
                 (if (or (zerop start)
                         (< stop n))
                     (rec 0 n nil)
                     (cons (subseq seq 0 start)
                           (rec start (+ start n) nil)))))
              (t (rec 0 n nil)))))))

;; useful-macros.lisp
(defmethod group ((lst list) n &key from-end)
  ;; (group '(1 2 3 4 5 6 7 8 9 0) 3) -> '((1 2 3) (4 5 6) (7 8 9) (0))
  (check-type n (integer 1))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (when lst
      (cond (from-end
             (let* ((nel   (length lst))
                    (start (mod nel n)))
               (cond
                 ((< nel n)     lst)
                 ((zerop start) (rec lst nil))
                 (t
                  (cons (subseq lst 0 start)
                        (rec (nthcdr start lst) nil))))))
            (t (rec lst nil))))))

;; useful-macros.lisp
(defun triples (&rest args)
  "Partition ARGS into a list of lists which length is 3."
  ;; (triples 1 2 3 4 5 6 7 8 9 0) -> '((1 2 3) (4 5 6) (7 8 9) (0))
  (group args 3))

(defun make-atomic-fixnum (&optional (init-value 0))
  "Return an object whose place can make compare-and-swap operation."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum value))
  #+ccl (make-array 1 :initial-element init-value)
  #-ccl (cons init-value nil))

(defmacro atomic-fixnum-place (atomic-fixnum)
  "Return the place/value of ATOMIC-FIXNUM."
  #+ccl `(svref ,atomic-fixnum 0)
  #-ccl `(car ,atomic-fixnum))

(defun atomic-fixnum-incf (atomic-fixnum &optional (delta 1))
  "Atomic incf ATOMIC-FIXNUM with DELTA and return the new value."
  (atomics:atomic-incf
      (atomic-fixnum-place atomic-fixnum)
      delta))
