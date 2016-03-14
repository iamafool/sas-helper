(load #p"c:/Dropbox/lisp/sasxpt/sasxpt.lisp")
(ql:quickload :ltk)
(load #p"c:/D/quicklisp/dists/quicklisp/software/ltk-20150113-http/tktable.lisp")
(ql:quickload :cffi)



(defpackage :sas-helper
  (:use :common-lisp :ltk :tktable :sasxpt :cffi)
  (:export #:main))

(in-package :sas-helper)

(define-foreign-library libreadsas
  (:windows "c:\\D\\GitHub\\ReadStat\\obj\\libreadsas.dll")
  (t (:default "libreadsas")))

(use-foreign-library libreadsas)


(defcstruct sas7bdat
  (obs_count :int)
  (var_count :int)
  (var_names :pointer)
  (var_labels :pointer)
  (var_formats :pointer)
  (var_types :pointer)
  (values :pointer)
  )

(defclass sd7 ()
  ((obs-count :initform nil :accessor sd7-obs-count)
   (var-count :initform nil :accessor sd7-var-count)
   (var-names :initform nil :accessor sd7-var-names)
   (var-labels :initform nil :accessor sd7-var-labels)
   (var-formats :initform nil :accessor sd7-var-formats)
   (var-types :initform nil :accessor sd7-var-types)
   (obs-records :initform nil :accessor sd7-obs-records)))

  
(defun read-sas7bdat (file)
  ""
  (let ((sd7-i-1 (make-instance 'sd7))) ; sd7 instance 01
    (with-foreign-object (x 'sas7bdat)
      (foreign-funcall "readstat_read_sas7bdat"
                       :string file
                       :pointer x
                       :int)
      (with-foreign-slots ((obs_count var_count var_names var_labels var_formats var_types values) x sas7bdat)
        (setf (sd7-obs-count sd7-i-1) obs_count)
        (setf (sd7-var-count sd7-i-1) var_count)
        (setf (sd7-var-names sd7-i-1)
              (loop for i from 0 below var_count
                 collect (mem-aref var_names :string i)))
        (setf (sd7-var-labels sd7-i-1)
              (loop for i from 0 below var_count
                 collect (mem-aref var_labels :string i)))
        (setf (sd7-var-formats sd7-i-1)
              (loop for i from 0 below var_count
                 collect (mem-aref var_formats :string i)))
        (setf (sd7-var-types sd7-i-1)
              (loop for i from 0 below var_count
                 collect (mem-aref (mem-aref var_types :pointer i) :int 0)))
        (setf (sd7-obs-records sd7-i-1)
              (loop for i from 0 below obs_count
                   collect
                   (loop for j from 0 below var_count
                        collect (mem-aref values :string (+ (* i var_count) j)))))

        ))
    sd7-i-1))

;; (setf sd7 (read-sas7bdat "c:\\D\\GitHub\\ReadStat\\src\\ra.sas7bdat"))
;; (sd7-var-count sd7)
;; (sd7-var-names sd7)
;; (sd7-obs-records sd7)

(defun table-data (var-names obs-records)
  (format t "Function table-data start.~%")
  (let ((result
         (append (list
                  (cons "#" (loop for c from 1 to (length var-names) collect c))
                  (cons "" var-names))
                 (loop
                    for obs in obs-records
                    for c from 1 to 100 by 1
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
    (format-wish "~a configure -selecttitle 1 -cursor arrow" table-path)

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
  (setf *debug-tk* nil)
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
            mf-open-xpt (make-menubutton mfile "Open Files" (lambda ()
                                                              (let ((files-to-open (open-sas-files))
                                                                    (file01 nil))
                                                                (if (and (listp files-to-open) (> (length files-to-open) 0))
                                                                    (progn
                                                                      (set-pwd files-to-open)
                                                                      (dolist (file01 files-to-open)
                                                                        (multiple-value-bind (xpt-name obs-records) (open-sas-file file01)
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
                                                                                    (notebook-select nb f1))))))))))
                                         :underline 0)
            mf-close-xpt (make-menubutton mfile "Close"
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
      (bind *tk* "<v>" (lambda (event) (declare (ignore event)) (open-sas-files))) ;todo this is test.
      (bind *tk* "<t>" (lambda (event) (declare (ignore event))))

      (pack nb :fill :both :expand t)
      (pack ce :side :bottom :fill :both)
      )))

  
(defun open-sas-files ()
  "Open SAS files"
  (get-open-file :filetypes '(("SAS datasets" "*.sas7bdat")
                              ("SAS XPT files" "*.xpt"))
                 :initialdir *pwd*
                 :multiple t))

(defun open-sas-file (file)
  "Open one SAS file"
  (let ((data nil)
        (file-ext (string-upcase (subseq file (1+ (position #\. file :from-end t))))))
    (format t "file is ~a.~%" file)
    (if (> (length file) 0)
        (progn
          (cond
            ((string= file-ext "SAS7BDAT")
             (setf data (read-sas7bdat file))
             (values file
                     (table-data (sd7-var-names data) (sd7-obs-records data))))
            ((string= file-ext "XPT")
             (setf data (read-xpt file))
             (values file
                     (table-data (xpt-var-names data) (xpt-obs-records data))))))
        (values nil nil))))

(defun set-pwd (files)
  "set current folder"
  (let ((file (car files)))
    (if (> (length file) 0)
        (setf *pwd* (subseq file 0 (1+ (position #\/ file :from-end t)))))))


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

