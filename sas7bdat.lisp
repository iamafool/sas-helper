(ql:quickload :cffi)
;; (ql:quickload :cffi-grovel)
;; (ql:quickload :cffi-libffi)

(defpackage :haven
  (:use :common-lisp :cffi))

(in-package :haven)
   
(define-foreign-library haven
  (t (:default "haven")))

;; 32 bit
(load-foreign-library "c:\\D\\GitHub\\ReadStat\\obj\\libreadsas.dll")

(defcstruct sas7bdat
  (obs_count :int)
  (var_count :int)
  (var_names :pointer)
  (var_labels :pointer)
  (var_formats :pointer)
  (var_types :pointer)
  (values :pointer)
  )

(with-foreign-object (x 'sas7bdat)
  (foreign-funcall "readstat_read_sas7bdat"
                 :string "c:\\D\\temp\\ra.sas7bdat"
                 :pointer x
                 :int)
  (with-foreign-slots ((obs_count var_count var_names var_labels var_formats var_types values) x sas7bdat)
    (format t "obs_count is ~a~%var_count is ~a~%" obs_count var_count)
    (format t "var names are ~a~%"
            (loop for i from 0 below var_count
               collect (mem-aref var_names :string i)))
    (format t "var labels are ~a~%"
            (loop for i from 0 below var_count
                 collect (mem-aref var_labels :string i)))
    (format t "var formats are ~a~%"
            (loop for i from 0 below var_count
               collect (mem-aref var_formats :string i)))
    (format t "var types are ~a~%"
            (loop for i from 0 below var_count
               collect (mem-aref (mem-aref var_types :pointer i) :int 0)))

    (format t "Values:~%")
    (loop for i from 0 below obs_count
         do
         (loop for j from 0 below var_count
            do
              (format t "~a~c" (mem-aref values :string (+ (* i var_count) j)) #\tab))
         (format t "~%"))
    
    ))
  

