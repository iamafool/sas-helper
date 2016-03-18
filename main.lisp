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



(defvar *pwd* nil)                      ;current folder
(defvar *titlerows* 2)                  ;the number of title rows
(defvar *titlecols* 1)                  ;the number of title columns
(defvar *no-rows-read* 500)             ;the maximum rows to display in the widget table


(defcstruct sas7bdat                    ;define the c struct that will be used by libreadstat
  (obs_count :int)
  (var_count :int)
  (var_names :pointer)
  (var_labels :pointer)
  (var_formats :pointer)
  (var_types :pointer)
  (values :pointer))

(defclass sas-data ()
  ((obs-count :initform 0 :accessor sas-data-obs-count)
   (var-count :initform 0 :accessor sas-data-var-count)
   (var-names :initform nil :accessor sas-data-var-names)
   (var-labels :initform nil :accessor sas-data-var-labels)
   (var-formats :initform nil :accessor sas-data-var-formats)
   (var-types :initform nil :accessor sas-data-var-types)
   (obs-records :initform nil :accessor sas-data-obs-records)))

(defclass nb-sctable ()
  ((filename :initform "" :accessor nb-sctable-filename)
   (data :initform nil :accessor nb-sctable-data)
   (view-start :initform 0 :accessor nb-sctable-view-start)
   (view-end :initform 0 :accessor nb-sctable-view-end)
   (sctable :initform nil :accessor nb-sctable-sctable)))

  
(defun read-sas7bdat (file)
  "use libreadstat to read sas dataset, sas7bdat"
  (let ((sas-data-i1 (make-instance 'sas-data))) ; sas-data instance 01
    (with-foreign-object (x 'sas7bdat)
      (foreign-funcall "readstat_read_sas7bdat"
                       :string file
                       :pointer x
                       :int)
      (with-foreign-slots ((obs_count var_count var_names var_labels var_formats var_types values) x sas7bdat)
        (setf (sas-data-obs-count sas-data-i1) obs_count)
        (setf (sas-data-var-count sas-data-i1) var_count)
        (setf (sas-data-var-names sas-data-i1)
              (loop for i from 0 below var_count
                 collect (mem-aref var_names :string i)))
        (setf (sas-data-var-labels sas-data-i1)
              (loop for i from 0 below var_count
                 collect (mem-aref var_labels :string i)))
        (setf (sas-data-var-formats sas-data-i1)
              (loop for i from 0 below var_count
                 collect (mem-aref var_formats :string i)))
        (setf (sas-data-var-types sas-data-i1)
              (loop for i from 0 below var_count
                 collect (mem-aref (mem-aref var_types :pointer i) :int 0)))
        (setf (sas-data-obs-records sas-data-i1)
              (loop for i from 0 below obs_count
                   collect
                   (loop for j from 0 below var_count
                        collect (mem-aref values :string (+ (* i var_count) j)))))))
    sas-data-i1))

;; (setf sas-data (read-sas7bdat "c:\\D\\GitHub\\ReadStat\\src\\ra.sas7bdat"))
;; (sas-data-var-count sas-data)
;; (sas-data-var-names sas-data)
;; (sas-data-obs-records sas-data)

(defun table-data (sas-data &key (row-titles '(no name)) (start 0) (end nil))
  (format t "Function table-data start.~%")
  (let* ((var-names (sas-data-var-names sas-data))
         (obs-records (sas-data-obs-records sas-data))
         (result
         (append (list
                  (cons "#" (loop for c from 1 to (length var-names) collect c))
                  (cons "" var-names))
                 (loop
                    for obs in (subseq obs-records start end)
                    for c from (1+ start) by 1
                    collect (cons c obs)))))
    (format t "Function table-data end.~%")
    result))

#|
(let* ((xpt01 (read-xpt "c:/dropbox/lisp/sasxpt/dm.xpt")) ;todo update
      (var-names (xpt-var-names xpt01))
      (obs-records (xpt-obs-records xpt01)))
  (table-data var-names obs-records))
|#

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
    (format-wish "~a configure -background white -foreground black -state disable -ellipsis ... -multiline 1" table-path)
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
                     
    ))


(defun main ()
  (setf *debug-tk* t)
  (let ((filename-hash (make-hash-table :test 'equal))
        (nb-sctable-hash (make-hash-table :test 'equal))
        ce nb menubar mfile mf-open-xpt mf-close-xpt sep1 mf-exit mhelp mf-about test)
    

    (with-ltk ()
      ;; define font
      (font-create 'myfont1 :size 10)

      (setf ce (make-instance 'entry :width 40) ;command entry
            nb (make-instance 'notebook)
            
            test (make-instance 'button :text "test")
            
            menubar (make-menubar)
            mfile (make-menu menubar "File" )
            mf-open-xpt (make-menubutton mfile "Open Files" (lambda ()
                                                              (let ((files-to-open (open-sas-files)))
                                                                (if (and (listp files-to-open) (> (length files-to-open) 0))
                                                                    (progn
                                                                      (set-pwd files-to-open)
                                                                      (dolist (file01 files-to-open)
                                                                        (let* ((data01 (open-sas-file file01))
                                                                               (nb-sctable-i1 (make-instance 'nb-sctable))
                                                                               (last-obs (min (sas-data-obs-count data01) *no-rows-read*))
                                                                               (nb-tab-text01 (subseq file01 (1+ (position #\/ file01 :from-end t))))
                                                                               (frame1 nil)
                                                                               (sctable1 nil))
                                                                          (setf (nb-sctable-view-start nb-sctable-i1) 0)
                                                                          (setf (nb-sctable-view-end nb-sctable-i1) last-obs)
                                                                          (setf (nb-sctable-data nb-sctable-i1) data01)
                                                                          
                                                                          (if (gethash file01 nb-sctable-hash)
                                                                              (notebook-select nb (master (nb-sctable-sctable (gethash file01 nb-sctable-hash)))) ;get frame from nb-sctable instance
                                                                              (progn
                                                                                (setf frame1 (make-instance 'frame :master nb)
                                                                                      sctable1 (make-instance 'scrolled-table
                                                                                                             :titlerows *titlerows*
                                                                                                             :titlecols *titlecols*
                                                                                                             :data (table-data data01 :start 0 :end last-obs)
                                                                                                             :master frame1))
                                                                                (configure-sctable sctable1)
                                                                                (setf (nb-sctable-sctable nb-sctable-i1) sctable1)
                                                                                (setf (gethash file01 nb-sctable-hash) nb-sctable-i1)
                                                                                (setf (gethash nb-tab-text01 filename-hash) file01)
                                                                                
                                                                                (pack sctable1 :fill :both :expand t)
                                                                                (notebook-add nb frame1 :text nb-tab-text01) 
                                                                                (configure mf-close-xpt 'state 'normal)
                                                                                (notebook-select nb frame1)))))))))
                                         :underline 0)
            mf-close-xpt (make-menubutton mfile "Close"
                                          (lambda ()
                                            (let ((tabname (notebook-tab nb "current" "text" "")))
                                              (remhash (gethash tabname filename-hash) nb-sctable-hash)
                                              (remhash tabname filename-hash)
                                              (if (= (hash-table-count filename-hash) 0)
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
      (bind *tk* "<n>" (lambda (event) (declare (ignore event))
                               (update-table-variable nb filename-hash nb-sctable-hash :pagedown t)))
      (bind *tk* "<p>" (lambda (event) (declare (ignore event))
                               (update-table-variable nb filename-hash nb-sctable-hash :pagedown nil)))
      (bind *tk* "<t>" (lambda (event) (declare (ignore event)) (test sctable1 nb filename-hash)))

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
        (cond
          ((string= file-ext "SAS7BDAT")
           (setf data (read-sas7bdat file)))
          ((string= file-ext "XPT")
           (setf data (read-xpt file)))))
    data))

(defun set-pwd (files)
  "set current folder"
  (let ((file (car files)))
    (if (> (length file) 0)
        (setf *pwd* (subseq file 0 (1+ (position #\/ file :from-end t)))))))



(defun about-box ()
  (format t "About~%")
  (finish-output))

(defun set-array (array-name array-data)
  "For TCL, array set name {}"
  (with-open-stream (s (make-string-output-stream))
    (format s "global ~a;array set ~a {~%" array-name array-name)
    (loop
       for row in array-data
       for i from 0 by 1
       do
         (loop
            for col in row
            for j from 0 by 1
            do
              (format s "~a,~a ~a~a~a~%" i j #\" col #\")))
    (format s "}~%")
    (get-output-stream-string s)))

;; (set-array "ws" '((5 10) (10 15)))

(defun update-table-variable (nb filename-hash nb-sctable-hash &key (pagedown t))
  (let* ((tabname (notebook-tab nb "current" "text" ""))
         (filename (gethash tabname filename-hash))
         (nb-sctable-i1 (gethash filename nb-sctable-hash))
         (variable-name (ltk::name (table (nb-sctable-sctable nb-sctable-i1))))
         (view-start (nb-sctable-view-start nb-sctable-i1))
         (view-end (nb-sctable-view-end nb-sctable-i1))
         (obs-count (sas-data-obs-count (nb-sctable-data nb-sctable-i1)))
         (new-start (if pagedown
                        (if (< view-end obs-count) view-end view-start)
                        (max 0 (- view-start *no-rows-read*))))
         (new-end (if pagedown
                      (min obs-count (+ view-end *no-rows-read*))
                      (max (min obs-count *no-rows-read*) (- view-end *no-rows-read*)))))

    ;;todo delete
    (format t "last obs is ~a ~%" view-end)
    (format t "new start is ~a, new end is ~a~%" new-start new-end)

    (if (not (or (and pagedown (= view-end obs-count)) ;pagedown and reach the end of sas data
                 (and (null pagedown) (= view-start 0)) ;pageup and reach the first obs of sas data
                 ))
        (progn
          (format-wish "global ~a; array unset ~a" variable-name variable-name)
          (send-wish (set-array variable-name
                                (table-data (nb-sctable-data nb-sctable-i1) :start new-start :end new-end)))
          (setf (nb-sctable-view-end nb-sctable-i1) new-end)
          (setf (nb-sctable-view-start nb-sctable-i1) new-start)))

  ))

(defun test (sctable nb filename-hash)
  (let ((x (window-x *tk*))
        (y (window-y *tk*))
        (table-path (widget-path (table sctable)))
        (tabname (notebook-tab nb "current" "text" ""))
        )
    
    ;; configure embedded window
    ;; (format-wish "~a window configure 1,1 -window ~a" table-path (widget-path test))
  ))

;; (main)
