(in-package :mcas-test)

(defun tstx (&optional (n 1000000))
  ;; should return '(2000000 1 2)
  (let ((a  (make-mcas-ref 1))
        (b  (make-mcas-ref 2))
        (ct 0))
    (bt:make-thread (lambda ()
                      (loop repeat n do
                        (loop until (mcas a 1 3
                                          b 2 4))
                        (incf ct)
                        (mcas a 3 5
                              b 4 6))))
    (loop repeat n do
      (loop until (mcas a 5 7
                        b 6 8))
      (incf ct)
      (mcas a 7 1
            b 8 2))
    (list ct (mcas-val a) (mcas-val b))))

(define-test basic-mcas :parent mcas
  (let* ((a  (make-mcas-ref 15))
         (b  (make-mcas-ref 16)))
    (mcas  a 15 32
           b 16 33)
    (is equal (list 32 33) (list (mcas-val a) (mcas-val b)))))

(define-test tstx :parent mcas
  (let ((n 1000000))
    (is equal
        (list (* 2 n) 1 2)
        (tstx n))))

(define-test race :parent mcas
  ;; this test will take quite a while, reduce "i" will reduce the time.
  (dotimes (i 10000)
    (let* ((num 1000) ; mcas threads num
           (a  (make-mcas-ref -1))
           (b  (make-mcas-ref -1))
           (c  (make-mcas-ref -1))
           (a-vals (loop for i below num collect i))
           (b-vals (loop for i in a-vals collect (+ i num)))
           (c-vals (loop for i in a-vals collect (+ i num num)))
           (threads (loop for new-aa in a-vals
                          for new-bb in b-vals
                          for new-cc in c-vals
                          ;; should re-bind, or the they will always point to the loop vars' last values
                          collect (let ((new-a new-aa)
                                        (new-b new-bb)
                                        (new-c new-cc))
                                    (bt:make-thread (lambda ()
                                                      (mcas a (mcas-val a) new-a
                                                            c (mcas-val c) new-c
                                                            b (mcas-val b) new-b)))))))
      (dolist (th threads)
        (bt:join-thread th))
      ;;(format t "~&a: ~d, b: ~d, c: ~d~%" (mcas-val a) (mcas-val b) (mcas-val c))
      (true (<= 0 (mcas-val a) num))
      (is = (mcas-val b) (+ num (mcas-val a)))
      (is = (mcas-val c) (+ num num (mcas-val a))))))
