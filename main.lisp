(ql:quickload :ltk)

(load #p"c:/Dropbox/lisp/sasxpt/sasxpt.lisp")
(load #p"c:/D/quicklisp/dists/quicklisp/software/ltk-20150113-http/tktable.lisp")

(defpackage :sas-helper
  (:use :common-lisp :ltk :tktable :sasxpt)
  (:export #:main))

(in-package :sas-helper)

(defun table-data (var-names obs-records)
  (format t "Function table-data start.~%")
  (let ((result
         (append (list
                  (cons "#" (loop for c from 1 to (length var-names) collect c))
                  (cons "" var-names))
                 (loop
                    for obs in obs-records
                    for c from 1 to 500 by 1
                    collect (cons c obs)))))
    (format t "Function table-data end.~%")
    result))

#|
(let* ((xpt01 (read-xpt "c:/dropbox/lisp/sasxpt/dm.xpt"))
      (var-names (xpt-var-names xpt01))
      (obs-records (xpt-obs-records xpt01)))
  (table-data var-names obs-records))
|#


(defvar frame-hash (make-hash-table :test 'equal))

(defun main ()
  (setf *debug-tk* nil)
  (with-ltk ()
    (let* ((ce (make-instance 'entry :width 40)) ;command entry
           (nb (make-instance 'notebook))
           (menubar (make-menubar))
           (mfile (make-menu menubar "File" ))
           (mf-open-xpt (make-menubutton mfile "Open .xpt"
                                         (lambda ()
                                           (let* ((f1 (make-instance 'frame :master nb))
                                                  (sctable nil)
                                                  (xpt-name1 nil))
                                             (multiple-value-bind (xpt-name obs-records) (open-xpt-file)
                                               (setf xpt-name1 (subseq xpt-name (1+ (position #\/ xpt-name :from-end t))))
                                               (setf sctable (make-instance 'scrolled-table
                                                                            :titlerows 2
                                                                            :titlecols 1
                                                                            :data obs-records
                                                                            :master f1))
                                               (setf (gethash xpt-name1 frame-hash) f1)
                                               (pack sctable :fill :both :expand t)
                                               (notebook-add nb f1 :text xpt-name1))))
                                         :underline 0))
           (mf-close-xpt (make-menubutton mfile "Close .xpt"
                                          (lambda ()
                                            (notebook-forget nb (gethash "f1" frame-hash)))))
           (sep1 (add-separator mfile))
           (mf-exit (make-menubutton mfile "Exit" (lambda () (setf *exit-mainloop* t))
                                     :underline 1
                                     :accelerator "Alt q"))
           (mhelp (make-menu menubar "Help" ))
           (mf-about (make-menubutton mhelp "About" #'about-box
                                     :underline 0))
           )

      (wm-title *tk* "SAS Helper")
      (minsize *tk* 400 300)
      (bind *tk* "<Alt-q>" (lambda (event) (declare (ignore event)) (setf *exit-mainloop* t)))
      (pack nb :fill :both :expand t)
      (pack ce :side :bottom :fill :both)
      )))

(defun open-xpt-file ()
  ""
  (let* ((file-to-open (get-open-file :filetypes '(("SAS XPT files" "*.xpt"))))
         (xpt01 (read-xpt file-to-open)))
    (values file-to-open
            (table-data (xpt-var-names xpt01) (xpt-obs-records xpt01)))))




;; (main)
;; (ltk::nbtest)
;; (ltk::ltktest)


(defun about-box ()
  (format t "About~%")
  (finish-output))
