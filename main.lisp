(ql:quickload :ltk)

(load #p"c:/Dropbox/lisp/sasxpt/sasxpt.lisp")
(load #p"c:/D/quicklisp/dists/quicklisp/software/ltk-20150113-http/tktable.lisp")

(defpackage :sas-helper
  (:use :common-lisp :ltk :tktable :sasxpt)
  (:export #:main))

(in-package :sas-helper)

(defun main ()
  (setf *debug-tk* nil)
  (with-ltk ()
    (let* ((xpt01 (sasxpt::read-xpt "c:/dropbox/lisp/sasxpt/dm.xpt"))
           (b (make-instance 'button
                            :text "Hello World!"
                            :command (lambda ()
                                       (do-msg "Bye!" :title "Hello World!")
                                       (setf *exit-mainloop* t))))
          (sctable (make-instance 'scrolled-table
				:titlerows 2
				:titlecols 2
				:data
				(cons (cons "*" (loop for c from 1 to 40 collect
                                   c))
				      (loop for r from 1 to 200
                         collect
                           (cons r
                                 (loop for c from 1 to 40 collect
                                      (* r c))))))))
      (pack b)
      (pack sctable :side :top :fill :both :expand t)
      (format t "7 * 8 is ~a~%" (car (subvals (table sctable) 7 8)))
      (finish-output))))


(main)
