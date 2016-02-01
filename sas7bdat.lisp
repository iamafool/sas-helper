(ql:quickload :cffi)

(defpackage :haven
  (:use :common-lisp :cffi))
   
(in-package :haven)
   
(define-foreign-library haven
  (t (:default "haven")))

;; 32 bit
(load-foreign-library "c:\\D\\GitHub\\ReadStat\\obj\\libreadsas.dll")

(defcstruct sas7bdat
  (obs_count :int)
  (var_count :int))


(defcallback handle_info :int ((obs_count :int) (var_count :int) (ctx :pointer))
     (setf (foreign-slot-value ctx 'sas7bdat 'obs_count) obs_count
           (foreign-slot-value ctx 'sas7bdat 'var_count) var_count)
     ;; (setf (mem-ref ctx :int) obs_count)
  0)


(defcallback handle_value :int ((obs_index :int) (var_index :int) (ctx :pointer))
  0)

(setf a (foreign-funcall "readstat_parser_init"
                         :pointer))

(foreign-funcall "readstat_set_info_handler"
                 :pointer a
                 :pointer (callback handle_info))

(foreign-funcall "readstat_set_value_handler"
                 :pointer a
                 :pointer (callback handle_value))


(with-foreign-object (x 'sas7bdat)
  (foreign-funcall "readstat_parse_sas7bdat"
                 :pointer a
                 :string "c:\\D\\temp\\ra.sas7bdat"
                 :pointer x
                 :pointer)
  (with-foreign-slots ((obs_count var_count) x sas7bdat)
    (list obs_count var_count)))
  
(let ((x (foreign-alloc :int)))
  (foreign-funcall "readstat_parse_sas7bdat"
                 :pointer a
                 :string "c:\\D\\temp\\ra.sas7bdat"
                 :pointer x
                 :pointer)
  (mem-ref x :int))

(foreign-funcall "readstat_parser_free"
                 :pointer a
                 :void)

