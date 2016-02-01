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
  (var_count :int)
  (var_names :pointer))

;; readstat_variable_t
;; readstat_value_t


;; typedef int (*readstat_info_handler)(int obs_count, int var_count, void *ctx);
(defcallback handle_info :int ((obs_count :int) (var_count :int) (ctx :pointer))
     (setf (foreign-slot-value ctx '(:struct sas7bdat) 'obs_count) obs_count
           (foreign-slot-value ctx '(:struct sas7bdat) 'var_count) var_count
           (foreign-slot-value ctx '(:struct sas7bdat) 'var_names) (foreign-alloc :string :count var_count))
     ;; (setf (mem-ref ctx :int) obs_count)
  0)

;; typedef int (*readstat_variable_handler)(int index, readstat_variable_t *variable, const char *val_labels, void *ctx);
(defcallback handle_variable :int ((index :int) (variable :pointer) (val_labels :pointer) (ctx :pointer))
  (setf (mem-aref (foreign-slot-value ctx '(:struct sas7bdat) 'var_names) :string index)
        (foreign-funcall "readstat_variable_get_name"
                         :pointer variable
                         :string)
         )
  0)

;; typedef int (*readstat_value_handler)(int obs_index, int var_index, readstat_value_t value, void *ctx);
(defcallback handle_value :int ((obs_index :int) (var_index :int) (ctx :pointer))
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



(with-foreign-object (x 'sas7bdat)
  (foreign-funcall "readstat_parse_sas7bdat"
                 :pointer a
                 :string "c:\\D\\temp\\ra.sas7bdat"
                 :pointer x
                 :pointer)
  (with-foreign-slots ((obs_count var_count var_names) x sas7bdat)
    (format t "obs_count is ~a~%var_count is ~a~%" obs_count var_count)
    (format t "var names are ~a~%"
            (loop for i from 0 below var_count
                 collect (mem-aref var_names :string i)))))
  
(foreign-funcall "readstat_parser_free"
                 :pointer a
                 :void)

