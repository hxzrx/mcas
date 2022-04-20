(in-package :mcas)


;; parse-body.lisp
(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (push (pop body) doc)) ;; Alexandira fails here...
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
(defun decl-us (args)
  (when (is-underscore? args)
    `((declare (ignore ,args)))))

;; xlambda.lisp
(defun us-conv (args)
  (cond ((eq nil args)  nil)
        ((symbolp args) `(&rest ,args))
        (t              args)))

(defun dotted-list-p (lst) ; by hxz, 20211229
  (if (listp lst)
      (cdr (last lst))
      lst))

(defun is-lambda-list-keyword (arg)
  (member arg lambda-list-keywords))

;; xlambda.lisp
(defun destr-lambda-list-p (args)
  (and (consp args)
       (or (eq (car args) '&whole)
           ;;(lw:dotted-list-p args)
           (dotted-list-p args) ; by hxz, 20211229
           (some 'consp (subseq args 0
                                (position-if #'is-lambda-list-keyword args))
                 ))))

;; xlambda.lisp
(defun wrap-assembly (name args &rest body)
  (if (destr-lambda-list-p args)
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
  (apply #'wrap-assembly 'lambda args body))

;; useful-macros.lisp
(defmethod group ((seq sequence) n &key from-end)
  (check-type n (integer 1))
  (let ((stop (length seq)))
    (labels ((rec (start end acc)
               (if (>= end stop)
                   (nreverse (cons (subseq seq start stop) acc))
                   (rec end (+ end n) (cons (subseq seq start end) acc))
                   )))
      (when (plusp stop)
        (cond (from-end
               (let ((start (mod stop n)))
                 (if (or (zerop start)
                         (< stop n))
                     (rec 0 n nil)
                     (cons (subseq seq 0 start)
                           (rec start (+ start n) nil))
                     )))

              (t
               (rec 0 n nil))
              )))))

(defmethod group ((lst list) n &key from-end)
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
                        (rec (nthcdr start lst) nil)))
                 )))

            (t
             (rec lst nil))
            ))))


;; useful-macros.lisp
(defun triples (&rest args)
    (group args 3))
