(in-package :mcas)

(defvar *mcas-index*  0)

(defstruct ref
  val)

(defmethod ref (obj)
  ;; we need this to be a defmethod for FSTM
  (make-ref
   :val obj))

(defstruct (mcas-ref
            (:include ref)
            (:constructor mcas-ref (val)))
  (id (incf *mcas-index*) :read-only t))

(defmethod compare ((a mcas-ref) (b mcas-ref))
  (- (mcas-ref-id a) (mcas-ref-id b)))

(defstruct (mcas-desc
            (:include ref)
            (:constructor %make-mcas-desc))
  triples)

(defun undecided-p (mdesc)
  (eq :undecided (ref-val mdesc)))

(defun successful-p (mdesc)
  (eq :successful (ref-val mdesc)))

(defstruct word-desc
  parent addr old new)

(defun make-mcas-desc (triples)
  (let ((desc (%make-mcas-desc
               :val :undecided)))
    (setf (mcas-desc-triples desc)
          (mapcar (lambda* ((mref old new))
                           (make-word-desc
                            :parent desc
                            :addr   mref
                            :old    old
                            :new    new))
                  triples))
    desc))

(defun read-helper (mref self)
  ;; mref must be an MCAS-REF
  (prog ()
   again
     (let ((content (ref-val mref)))
       (if (word-desc-p content)
           (let ((parent (word-desc-parent content)))
             (if (and (not (eq parent self))
                      (undecided-p parent))
                 (progn
                   (mcas-help parent)
                   (go again))
                 ;; else
                 (return (values content
                                 (if (successful-p parent)
                                     (word-desc-new content)
                                     (word-desc-old content))))
                 ))
           ;; else - content was not an word descriptor, just a value
           (return (values content content)))
       )))

(defun mcas-read (mref)
  ;; mref must be an MCAS-REF
  (multiple-value-bind (content value)
      (read-helper mref nil)
    (declare (ignore content))
    value))

(defun mcas-help (mdesc)
  ;; minimum CAS algorithm, for N locations, needs only N+1 CAS
  (declare (mcas-desc mdesc))
  (atomics:cas
   (ref-val mdesc) :undecided
   (if (every (lambda (wdesc)
                (declare (word-desc wdesc))
                (prog ()
                 again
                   (multiple-value-bind (content value)
                       (read-helper (word-desc-addr wdesc) mdesc)
                     (return (or (eq content wdesc)
                                 (and (eq value (word-desc-old wdesc))
                                      (undecided-p mdesc)
                                      (or (atomics:cas
                                           (ref-val (word-desc-addr wdesc))
                                           content wdesc)
                                          (go again))
                                      )))
                     )))
              (mcas-desc-triples mdesc))
       :successful
       :failed))
  (successful-p mdesc))

(defun mcas (&rest triples)
  ;; triples - a sequence of (ref old new) as would be suitable for
  ;; CAS. But each ref must be a total-order MCAS-REF.
  (mcas-help (make-mcas-desc
              (sort (apply 'triples triples)
                    '<
                    :key (lambda (tup)
                           (mcas-ref-id (first tup)))
                    ))))

(defmethod val ((m mcas-ref))
  ;; to get the current value of an mcas-ref
  ;; always use either ref:val or mcas-read
  (mcas-read m))

(defmethod cas-object ((m mcas-ref) old new)
  (mcas m old new))

(let* ((a  (mcas-ref 15))
       (b  (mcas-ref 16)))
  (mcas  a 15 32
         b 16 33)
  (list (val a) (val b)))
