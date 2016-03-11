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

(defcstruct :sas7bdat
  (obs_count :int)
  (var_count :int)
  (var_names :pointer)
  (var_labels :pointer)
  (var_formats :pointer)
  (var_types :pointer)
  (values :pointer)
  )


(defcenum  :readstat_types_t
  :READSTAT_TYPE_STRING
  :READSTAT_TYPE_CHAR
  :READSTAT_TYPE_INT16
  :READSTAT_TYPE_INT32
  :READSTAT_TYPE_FLOAT
  :READSTAT_TYPE_DOUBLE
  :READSTAT_TYPE_LONG_STRING)

(defcunion :value
  (char_value :char)
  (float_value :float)
  (double_value :double)
  (i16_value :int16)
  (i32_value :int32)
  (string_value :pointer))


;; readstat_variable_t
;; readstat_value_t
(defcstruct (:readstat_value_t :size 24)
  (v (:pointer (:union :value)))
  (type :int :offset 8)
  (tag :char))

(foreign-type-size '(:struct :readstat_value_t))
  ;; (is_system_missing :unsigned-int)
  ;; (is_considered_missing :unsigned-int))


;; typedef int (*readstat_info_handler)(int obs_count, int var_count, void *ctx);
(defcallback handle_info :int ((obs_count :int) (var_count :int) (ctx :pointer))
     (setf (foreign-slot-value ctx '(:struct :sas7bdat) 'obs_count) obs_count
           (foreign-slot-value ctx '(:struct :sas7bdat) 'var_count) var_count
           (foreign-slot-value ctx '(:struct :sas7bdat) 'var_names) (foreign-alloc :string :count var_count)
           (foreign-slot-value ctx '(:struct :sas7bdat) 'var_labels) (foreign-alloc :string :count var_count)
           (foreign-slot-value ctx '(:struct :sas7bdat) 'var_formats) (foreign-alloc :string :count var_count)
           (foreign-slot-value ctx '(:struct :sas7bdat) 'var_types) (foreign-alloc :int :count var_count)
           (foreign-slot-value ctx '(:struct :sas7bdat) 'values) (foreign-alloc :string :count (* var_count obs_count)))
  0)

(defmacro aref-sas-vars (ctx-pointer item type index)
  `(mem-aref (foreign-slot-value ,ctx-pointer '(:struct :sas7bdat) ,item) ,type ,index))
;; (aref-sas-vars ctx 'var_names)

;; typedef int (*readstat_variable_handler)(int index, readstat_variable_t *variable, const char *val_labels, void *ctx);
(defcallback handle_variable :int ((index :int) (variable :pointer) (val_labels :string) (ctx :pointer))
  (setf (aref-sas-vars ctx 'var_names :string index) (foreign-funcall "readstat_variable_get_name" :pointer variable :string)
        (aref-sas-vars ctx 'var_labels :string index) (foreign-funcall "readstat_variable_get_label" :pointer variable :string)
        (aref-sas-vars ctx 'var_formats :string index) (or (foreign-funcall "readstat_variable_get_format" :pointer variable :string) "")
        (aref-sas-vars ctx 'var_types :int index) (foreign-funcall "readstat_variable_get_type" :pointer variable :int)
        )
  (format t "The return of readstat_variable_get_format is ~a~%" (foreign-funcall "readstat_variable_get_format" :pointer variable :string))
  0)

;; typedef int (*readstat_value_handler)(int obs_index, int var_index, readstat_value_t value, void *ctx);
(defcallback handle_value :int ((obs_index :int) (var_index :int) (value (:pointer (:struct :readstat_value_t))) (ctx :pointer))
  (let ((v_type (foreign-funcall "readstat_value_type" (:struct :readstat_value_t) value :readstat_types_t)))
    (format t "obs_index is ~a, var_index is ~a, the type is ~a, and the value is ~a~%" obs_index var_index v_type
            (if (= v_type 0) (foreign-funcall "readstat_string_value" (:struct :readstat_value_t) value :pointer)
                "not string")))
  0)

;; typedef int (*readstat_value_label_handler)(const char *val_labels, readstat_value_t value, const char *label, void *ctx);
(defcallback handle_value_label :int ((val_labels :pointer)  (value :pointer) (label :pointer) (ctx :pointer))
    0)

;; typedef void (*readstat_error_handler)(const char *error_message, void *ctx);
(defcallback handle_error :int ((error_message :pointer) (ctx :pointer))
    0)


(setf a (foreign-funcall "readstat_parser_init"
                         :pointer))

(foreign-funcall "readstat_set_info_handler"
                 :pointer a
                 :pointer (callback handle_info)
                 :int)

(foreign-funcall "readstat_set_variable_handler"
                 :pointer a
                 :pointer (callback handle_variable)
                 :int)

(foreign-funcall "readstat_set_value_handler"
                 :pointer a
                 :pointer (callback handle_value)
                 :int)

(foreign-funcall "readstat_set_value_label_handler"
                 :pointer a
                 :pointer (callback handle_value_label)
                 :int)

(foreign-funcall "readstat_set_error_handler"
                 :pointer a
                 :pointer (callback handle_error)
                 :int)



(with-foreign-object (x :sas7bdat)
  (foreign-funcall "readstat_parse_sas7bdat"
                 :pointer a
                 :string "c:\\D\\temp\\ra.sas7bdat"
                 :pointer x
                 :pointer)
  (with-foreign-slots ((obs_count var_count var_names var_labels var_formats var_types) x :sas7bdat)
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
               collect (mem-aref var_types :int i)))
    ))
  
(foreign-funcall "readstat_parser_free"
                 :pointer a
                 :void)

