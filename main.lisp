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


(defvar *pwd* nil)                      ;current folder
(defvar *titlerows* 2)
(defvar *titlecols* 1)

;; get the cells with tag "mysel"
(defun get-cell-mysel (table-path)
  (format-wish "senddatastrings [~a tag cell mysel]" table-path)
  (ltk::read-data))

(defmacro configure-sctable (sctable)
  "Configure scrolled table"
  `(let ((table-path (widget-path (table ,sctable)))
         (table-name (ltk::name ,sctable)))

     ;; define mytitle
    ;; (format-wish "~a tag row mytitle 0 1" table-path)
    ;; (format-wish "~a tag col mytitle 0" table-path)

    ;; configure title
    (format-wish "~a tag configure title -relief groove -background gray94 -foreground black -anchor center"
                 table-path)

    ;; configure mysel
    (format-wish "~a tag configure mysel -background systemHighlight" table-path)
    (format-wish "~a tag raise mysel" table-path)
    
    ;; configure scrolled table
    (format-wish "~a configure -background white -foreground black -state disabled -ellipsis ... -multiline 1" table-path)
    (format-wish "~a configure -selecttype cell -selectmode extended -anchor w -ipadx 2 -pady 2 -font myfont1" table-path)
    (format-wish "~a configure -selecttitle 1" table-path)

    (ltk::add-callback (format nil "~a-bc" table-name) ;tktable browsecommand callback
                       (lambda ()
                         (let ((r (index-row table-path "active"))
                               (c (index-col table-path "active"))
                               (c-mysel (get-cell-mysel table-path)))
                           (if (not (null c-mysel))
                               (format-wish "~a tag cell {} ~{~a ~}" table-path c-mysel))
                           (if (/= c 0) (format-wish "~a tag cell mysel 0,~a" table-path c))
                           (if (/= r 0) (format-wish "~a tag cell mysel ~a,0" table-path r))
                           )))

    (format-wish "~a configure -browsecommand {callback ~a}"
                 table-path (format nil "~a-bc" table-name))
                     
                                                  
    ;; configure embedded window
    (format-wish "~a window configure 1,1 -window ~a" table-path (widget-path test))

    ))


(defun main ()
  (setf *debug-tk* t)
  (let ((frame-hash (make-hash-table :test 'equal))
        (xpt-hash (make-hash-table :test 'equal))
        ce nb menubar mfile mf-open-xpt mf-close-xpt sep1 mf-exit mhelp mf-about f1 sctable test)
    

    (with-ltk ()
      ;; define font
      (font-create 'myfont1 :size 10)

      (setf ce (make-instance 'entry :width 40) ;command entry
            nb (make-instance 'notebook)
            
            test (make-instance 'button :text "test")
            
            menubar (make-menubar)
            mfile (make-menu menubar "File" )
            mf-open-xpt (make-menubutton mfile "Open .xpt" (lambda ()
                                                             (multiple-value-bind (xpt-name obs-records) (open-xpt-file)
                                                               (if xpt-name
                                                                   (if (gethash xpt-name frame-hash)
                                                                       (notebook-select nb (gethash xpt-name frame-hash))
                                                                       (let* ((xpt-name1 (subseq xpt-name (1+ (position #\/ xpt-name :from-end t)))))
                                                                         (setf f1 (make-instance 'frame :master nb)
                                                                               sctable (make-instance 'scrolled-table
                                                                                                      :titlerows *titlerows*
                                                                                                      :titlecols *titlecols*
                                                                                                      :data (and obs-records)
                                                                                                      :master f1))
                                                                         (configure-sctable sctable)
                                                                         (setf (gethash xpt-name frame-hash) f1)
                                                                                                      (setf (gethash xpt-name1 xpt-hash) xpt-name)
                                                                                                      
                                                                         (pack sctable :fill :both :expand t)
                                                                         (notebook-add nb f1 :text xpt-name1) 
                                                                         (configure mf-close-xpt 'state 'normal)
                                                                         (notebook-select nb f1))))))
                                         :underline 0)
            mf-close-xpt (make-menubutton mfile "Close .xpt"
                                          (lambda ()
                                            (let ((tabname (notebook-tab nb "current" "text" "")))
                                              (remhash (gethash tabname xpt-hash) frame-hash)
                                              (remhash tabname xpt-hash)
                                              (if (= (hash-table-count xpt-hash) 0)
                                                  (configure mf-close-xpt 'state 'disable))
                                              (format-wish "~a forget current" (widget-path nb))))
                                          :state 'disable)
            sep1 (add-separator mfile)
            mf-exit (make-menubutton mfile "Exit" (lambda ()
                                                    (setf *exit-mainloop* t))
                                     :underline 1
                                     :accelerator "Alt q")
            mhelp (make-menu menubar "Help" )
            mf-about (make-menubutton mhelp "About" #'about-box
                                      :underline 0))

      (wm-title *tk* "SAS Helper")
      (minsize *tk* 400 300)
      (bind *tk* "<Alt-q>" (lambda (event) (declare (ignore event)) (setf *exit-mainloop* t)))
      (bind *tk* "<v>" (lambda (event) (declare (ignore event)) (open-xpt-file))) ;todo this is test.
      (bind *tk* "<t>" (lambda (event) (declare (ignore event))))

      (pack nb :fill :both :expand t)
      (pack ce :side :bottom :fill :both)
      )))

  
(defun open-xpt-file ()
  ""
  (let* ((file-to-open (get-open-file :filetypes '(("SAS XPT files" "*.xpt"))
                                      :initialdir *pwd*))
         (xpt01 nil))
    (format t "file-to-open is ~a.~%" (length file-to-open))
    (if (> (length file-to-open) 0)
        (progn
          ;; set current folder
          (setf *pwd* (subseq file-to-open 0 (1+ (position #\/ file-to-open :from-end t))))
          (setf xpt01 (read-xpt file-to-open))
          (values file-to-open
                  (table-data (xpt-var-names xpt01) (xpt-obs-records xpt01))))
        (values nil nil))))




;; (main)
;; (ltk::nbtest)
;; (ltk::ltktest)


(defun about-box ()
  (format t "About~%")
  (finish-output))


(defun test (sctable)
  (let ((x (window-x *tk*))
        (y (window-y *tk*)))
    (format-wish "~a configure -ellipsis ~a" (widget-path (table sctable)) "...")

  ))

