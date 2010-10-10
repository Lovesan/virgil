;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2010, Dmitry Ignatiev <lovesan.ru@gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:virgil)

(defvar *struct-type-hash* (make-hash-table :test #'eq))
(defvar *union-type-hash* (make-hash-table :test #'eq))

(define-aggregate-type struct-type ()
  ((slots :initform nil
          :initarg :slots
          :reader struct-slots)
   (size :initform 0
         :initarg :size
         :reader struct-size)
   (size-no-padding :initform 0
                    :initarg :size-no-padding
                    :reader struct-size-no-padding)
   (align :initform 1
          :initarg :align
          :reader struct-align))
  (:lisp-type (type) 'hash-table)
  (:prototype (type)
    (let ((ht (make-hash-table :test #'eq)))
      (loop :for (name slot-type) :in (struct-slots type)
        :do (setf (gethash name ht)
                  (prototype slot-type)))
      ht))
  (:prototype-expansion (type)
    (with-gensyms (ht)
      `(let ((,ht (make-hash-table :test #'eq)))
         ,@(loop :for (name slot-type) :in (struct-slots type)
             :collect `(setf (gethash ',name ,ht)
                             ,(expand-prototype slot-type)))
         ,ht)))
  (:fixed-size (type) (struct-size type))
  (:align (type) (struct-align type))
  (:reader (pointer out type)
    (let* ((result (if (null out)
                     (prototype type)
                     out)))
      (declare (type hash-table result))
      (loop :for (name slot-type offset) :in (struct-slots type)
        :do (setf (gethash name result)
                  (read-value (&+ pointer offset)
                              (gethash name result)
                              slot-type)))
      result))
  (:reader-expansion (pointer out type)
    (with-gensyms (result)
      (once-only (pointer out)
        `(let ((,result (if (null ,out)
                          ,(expand-prototype type)
                          ,out)))
           (declare (type hash-table ,result)
                    (type pointer ,pointer))
           ,@(loop :for (name slot-type offset) :in (struct-slots type)
               :collect `(setf (gethash ',name ,result)
                               ,(expand-read-value
                                  `(&+ ,pointer ,offset)
                                  `(gethash ',name ,result)
                                  slot-type)))
           ,result))))
  (:writer (value pointer type)
    (loop :for (name slot-type offset) :in (struct-slots type)
      :do (write-value (gethash name value)
                       (&+ pointer offset)
                       slot-type))
    pointer)
  (:writer-expansion (value-form pointer-form type)
    (with-gensyms (pointer value)
      `(let ((,pointer ,pointer-form)
             (,value ,value-form))
         (declare (type pointer ,pointer)
                  (type hash-table ,value))
         ,@(loop :for (name slot-type offset) :in (struct-slots type)
             :collect (expand-write-value
                        `(gethash ',name ,value)
                        `(&+ ,pointer ,offset)
                        slot-type))
         ,pointer)))
  (:cleaner (pointer value type)
    (loop :for (name slot-type offset) :in (struct-slots type)
      :do (cleanup-value (&+ pointer offset)
                         (gethash name value)
                         slot-type)))
  (:cleaner-expansion (pointer-form value-form type)
    (with-gensyms (pointer value)
      `(let ((,pointer ,pointer-form) (,value ,value-form))
         (declare (type pointer ,pointer)
                  (type hash-table ,value)
                  (ignorable ,pointer ,value))
         ,@(loop :for (name slot-type offset) :in (struct-slots type)
             :collect (expand-cleanup-value
                        `(& ,pointer ,offset)
                        `(gethash ',name ,value)
                        slot-type))))))

(defun offsetof (type slot-name)
  (declare (type symbol slot-name))
  (let ((type (parse-typespec type)))
    (unless (typep type 'struct-type)
      (error "~s is not a structure type"
             (unparse-type type)))
    (loop :for (name slot-type offset) :in (struct-slots type)
      :when (eq name slot-name) :do (return offset)
      :finally (error "Structure type ~s has no slot named ~s"
                      (unparse-type type)
                      slot-name))))

(define-compiler-macro offsetof (&whole form type slot-name)
  (multiple-value-bind
      (type constantp)
      (eval-if-constantp type)
    (if constantp
      (let ((type (parse-typespec type)))
        (unless (typep type 'struct-type)
          (error "~s is not a structure type"
                 (unparse-type type)))
        (multiple-value-bind
            (slot-name constantp) (eval-if-constantp slot-name)
          (if constantp
            (let ((slot (progn
                          (check-type slot-name symbol)
                          (find slot-name (struct-slots type)
                                :key #'first :test #'eq))))
              (if (null slot)
                (error "Structure type ~s has no slot named ~s"
                       slot-name (unparse-type type))
                (third slot)))
            (once-only (slot-name)
              `(progn
                 (check-type ,slot-name symbol)
                 (case ,slot-name
                   ,@(loop :for (name slot-type offset) :in (struct-slots type)     
                       :collect `(,name ,offset))
                   (T (error "Structure type ~s has no slot named ~s"
                             ',(unparse-type type)
                             ,slot-name))))))))
      form)))

(define-aggregate-type named-struct-type (struct-type)
  ((name :initform nil
         :initarg :name
         :reader struct-name)
   (conc-name :initform nil
              :initarg :conc-name
              :reader struct-conc-name)
   (package :initarg :package
            :initform (symbol-value '*package*)
            :reader struct-package)
   (included :initform nil
             :initarg :include
             :reader struct-included)
   (ctor-name :initform nil
              :initarg :ctor-name
              :reader struct-ctor-name))
  (:lisp-type (type) (struct-name type))
  (:prototype (type)
    (funcall (struct-ctor-name type)))
  (:prototype-expansion (type)
    `(,(struct-ctor-name type)))
  (:reader (pointer out type)
    (let ((result (if (null out)
                    (prototype type)
                    out))
          (conc-name (struct-conc-name type)))
      (when (struct-included type)
        (read-value pointer result (struct-included type)))
      (loop :for (slot-name slot-type offset) :in (struct-slots type)
        :for accessor = (intern (format nil "~a~a" conc-name slot-name)
                                (struct-package type))
        :do (funcall (fdefinition `(setf ,accessor))
                     (read-value (&+ pointer offset)
                                 (funcall accessor result)
                                 slot-type)
                     result))
      result))
  (:reader-expansion (pointer out type)
    (with-gensyms (result)
      (once-only (out pointer)
        `(let ((,result (if (null ,out)
                          (,(struct-ctor-name type))
                          ,out)))
          (declare (type pointer ,pointer)
                   (type ,(lisp-type type) ,result))
           ,(when (struct-included type)
              (expand-read-value pointer result (struct-included type)))
           ,@(loop :with conc-name = (struct-conc-name type)
               :for (slot-name slot-type offset) :in (struct-slots type)
               :for accessor = (intern (format nil "~a~a" conc-name slot-name)
                                       (struct-package type))
               :collect `(setf (,accessor ,result)
                               ,(expand-read-value
                                  `(&+ ,pointer ,offset)
                                  `(,accessor ,result)
                                  slot-type)))
           ,result))))
  (:writer (value pointer type)
    (when (struct-included type)
      (write-value value pointer (struct-included type)))
    (loop :with conc-name = (struct-conc-name type)
      :for (slot-name slot-type offset) :in (struct-slots type)
      :for accessor = (intern (format nil "~a~a" conc-name slot-name)
                              (struct-package type))
      :do (write-value (funcall accessor value)
                       (&+ pointer offset)
                       slot-type)))
  (:writer-expansion (value-form pointer-form type)
    (with-gensyms (pointer value)
      `(let ((,pointer ,pointer-form) (,value ,value-form))
         (declare (type pointer ,pointer)
                  (type ,(lisp-type type) ,value))
         ,(when (struct-included type)
            (expand-write-value value pointer (struct-included type)))
         ,@(loop :with conc-name = (struct-conc-name type)
             :for (slot-name slot-type offset) :in (struct-slots type)
             :for accessor = (intern (format nil "~a~a" conc-name slot-name)
                                     (struct-package type))
             :collect (expand-write-value
                        `(,accessor ,value)
                        `(&+ ,pointer ,offset)
                        slot-type))
         ,pointer)))
  (:cleaner (pointer value type)
    (when (struct-included type)
      (cleanup-value pointer value (struct-included type)))
    (loop :with conc-name = (struct-conc-name type)
      :for (slot-name slot-type offset) :in (struct-slots type)
      :for accessor = (intern (format nil "~a~a" conc-name slot-name)
                              (struct-package type))
      :do (cleanup-value (&+ pointer offset)
                         (funcall accessor value)
                         slot-type)))
  (:cleaner-expansion (pointer-form value-form type)
    (with-gensyms (pointer value)
      `(let ((,pointer ,pointer-form)
             (,value ,value-form))
         (declare (type pointer ,pointer)
                  (type ,(lisp-type type) ,value)
                  (ignorable ,pointer ,value))
         ,(when (struct-included type)
            (expand-cleanup-value pointer value (struct-included type)))
         ,@(loop :with conc-name = (struct-conc-name type)
             :for (slot-name slot-type offset) :in (struct-slots type)
             :for accessor = (intern (format nil "~a~a" conc-name slot-name)
                                     (struct-package type))
             :collect (expand-cleanup-value
                        `(&+ ,pointer ,offset)
                        `(,accessor ,value)
                        slot-type))))))

(defun align-offset (offset align)
  (+ offset (mod (- align (mod offset align))
                 align)))

(defun parse-struct-slots (included slots named align-supplied packed)
  (loop :with current-offset = (if included
                                 (struct-size-no-padding included)
                                 0)
    :with total-align = (or align-supplied
                            (if included
                              (struct-align included)
                              1))
    :with names = (if included
                    (mapcar #'car (struct-slots included))
                    '())
    :for slot-spec :in slots
    :collect (destructuring-bind
                 (name type &rest rest) slot-spec
               (assert (and (symbolp name)
                            (not (constantp name)))
                   (name)
                 "Invalid slot name: ~s" slot-spec)
               (when (member name names :test #'eq)
                 (error "Duplicate slot names in struct"))
               (let ((type (parse-typespec type)))
                 (destructuring-bind
                     (&key (align (if packed
                                    1
                                    (compute-alignment type))
                                  align-p)
                      (offset (align-offset current-offset align))
                      (size (compute-fixed-size type))
                      (initform (when named (expand-prototype type)) initform-p))
                     rest
                   (check-type size non-negative-fixnum)
                   (check-type align non-negative-fixnum)
                   (check-type offset non-negative-fixnum)
                   (when (and (not named) initform-p)
                     (error "Unnamed structs don't support :initforms for slots"))
                   (when (and align-p packed)
                     (error
                       "You should not supply both :packed struct option and :align slot option ~s"
                       slot-spec))
                   (setf current-offset (+ offset size)
                         total-align (max align total-align))                   
                   (list name type offset align size initform))))
    :into slots
    :finally (return (values slots
                             (if packed
                               current-offset
                               (align-offset current-offset total-align))
                             current-offset
                             (or align-supplied total-align)))))

(define-type-parser struct (options &rest slots)
  (destructuring-bind
      (&key align packed) options
    (check-type align (or null positive-fixnum))
    (multiple-value-bind
        (slots size size-no-padding align)
        (parse-struct-slots nil slots nil align packed)
      (make-instance 'struct-type
        :slots slots
        :size size
        :size-no-padding size-no-padding
        :align align))))

(defmethod unparse-type ((type struct-type))
  `(struct (:align ,(struct-align type))
           ,@(loop :for (name slot-type offset align)
               :in (struct-slots type)
               :collect `(,name ,(unparse-type slot-type)
                                :offset ,offset
                                :align ,align))))

(defmethod unparse-type ((type named-struct-type))
  (struct-name type))

(defun notice-struct-definition
    (name ctor-name conc-name include align packed slots)
  (setf (gethash name *struct-type-hash*)
        (let ((included (when include
                          (or (gethash include *struct-type-hash*)
                              (error "Undefined structure type: ~s"
                                     include)))))
          (multiple-value-bind
              (slots size size-no-padding align)
              (parse-struct-slots included slots name align packed)
            (make-instance 'named-struct-type
              :name name
              :ctor-name ctor-name
              :conc-name conc-name
              :size size
              :include included
              :size-no-padding size-no-padding
              :align align
              :slots slots)))))

(defun slots->defstruct-slots (slots)
  (loop :for spec :in slots
    :collect (let* ((name (car spec))
                    (typespec (second spec))
                    (type (parse-typespec typespec)))
               (destructuring-bind
                   (&key (initform (expand-prototype type))
                    &allow-other-keys)
                   (cddr spec)
                 (list name initform :type (lisp-type type))))))

(defmacro define-struct (name-and-options &rest slots)
  (let* ((name-and-options (ensure-list name-and-options))
         (name (first name-and-options))
         (ctor-name (gensym "CONSTRUCTOR")))
    (assert (and (symbolp name)
                 (not (constantp name)))
        (name))
    (destructuring-bind
        (&key constructor
         (copier nil copier-p)
         (conc-name (format nil "~a-" name))
         (type nil type-p)
         (align nil align-p)
         (packed nil)
         (include nil include-p)
         (predicate (intern (format nil "~a-~a" name 'p)) predicate-p)
         (print-object nil print-object-p)
         (print-function nil print-function-p))
        (flatten-options (rest name-and-options))
      (when align-p (assert (typep align 'non-negative-fixnum) (align)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (notice-struct-definition
           ',name ',ctor-name ',conc-name ',(when include-p include)
           ',(when align-p align) ',packed ',slots)
         (declaim (inline ,ctor-name))
         (defstruct (,name
                      (:conc-name ,conc-name)
                      (:constructor ,ctor-name)
                      (:constructor ,@(ensure-list constructor))
                      ,@(when copier-p
                          `((:copier ,copier)))
                      ,@(when include-p
                          `((:include ,include)))
                      ,@(when predicate-p
                          `((:predicate ,predicate)))
                      ,@(when print-object-p
                          `((:print-object ,print-object)))
                      ,@(when print-function-p
                          `((:print-function ,print-function)))
                      ,@(when type-p
                          `((:type ,type))))
           ,@(slots->defstruct-slots slots))
         ,@(when type-p
             `((deftype ,name ()
                 ',(if (or (eq type 'vector)
                           (and (consp type)
                                (eq (car type) 'vector)))
                     (destructuring-bind
                         (&optional (elt-type t))
                         (rest (ensure-list type))
                       `(simple-array ,elt-type (*)))
                     type))
               (defun ,predicate (object)
                 (typep object ',name))))
         (define-type-parser ,name ()
           (gethash ',name *struct-type-hash*))
         ',name))))

(defun union-default-type (type)
  (lastcar (ensure-list (lisp-type type))))

(define-aggregate-type union-type (struct-type)
  ()
  (:lisp-type (type)
    `(or ,@(mapcar (lambda (slot &aux (type (second slot)))                     
                     (lisp-type type))
             (struct-slots type))
         ,(call-next-method type)))
  (:reader (pointer out type)
    (if (null out)
      (call-next-method pointer out type)
      (loop :for (name slot-type) :in (struct-slots type)
        :do (when (typep out (lisp-type slot-type))
              (return (read-value pointer out slot-type)))
        :finally (if (typep out (union-default-type type))
                   (return (call-next-method pointer out type))
                   (error "~s is invalid value for union type ~s"
                          out (unparse-type type))))))
  (:reader-expansion (pointer out type)
    (once-only (pointer out)      
      `(if (null ,out)
         ,(call-next-method pointer out type)
         (etypecase ,out
           ,@(loop :for (name slot-type) :in (struct-slots type)
               :collect `(,(lisp-type slot-type)
                          ,(expand-read-value
                             pointer
                             out
                             slot-type)))
           (,(union-default-type type)
            ,(call-next-method pointer out type))))))
  (:writer (value pointer type)
    (loop :for (name slot-type) :in (struct-slots type)
      :do (when (typep value (lisp-type slot-type))
            (return (write-value value pointer slot-type)))
      :finally (if (typep value (union-default-type type))
                 (call-next-method value pointer type)
                 (error "~s is invalid value for union type ~s"
                        value (unparse-type type)))))
  (:writer-expansion (value pointer type)
    (once-only (value pointer)
      `(etypecase ,value
         ,@(loop :for (name slot-type) :in (struct-slots type)
             :collect `(,(lisp-type slot-type)
                        ,(expand-write-value
                           value
                           pointer
                           slot-type)))
         (,(union-default-type type)
          ,(call-next-method value pointer type)))))
  (:cleaner (pointer value type)
    (loop :for (name slot-type) :in (struct-slots type)
      :do (when (typep value (lisp-type slot-type))
            (return (cleanup-value pointer value slot-type)))
      :finally (if (typep value (union-default-type type))
                 (call-next-method pointer value type)
                 (error "~s is invalid value for union type ~s"
                        value (unparse-type type)))))
  (:cleaner-expansion (pointer value type)
    (once-only (pointer value)
      `(etypecase ,value
         ,@(loop :for (name slot-type) :in (struct-slots type)
             :collect `(,(lisp-type slot-type)
                        ,(expand-cleanup-value
                           pointer
                           value
                           slot-type)))
         (,(union-default-type type) ,(call-next-method pointer value type))))))

;;Time for CLOS magic to come
(define-aggregate-type named-union-type (union-type named-struct-type)
  ())

;;Aargh!! Delicious copypasta!!
(defun parse-union-slots (included slots named align-supplied)
  (loop :with total-size = (if included (struct-size-no-padding included) 0)
    :with total-align = (or align-supplied
                            (if included (struct-align included) 1))
    :with names = (if included (mapcar #'car (struct-slots included)) '())
    :for slot-spec :in slots
    :collect (destructuring-bind
                 (name type &rest rest) slot-spec
               (assert (and (symbolp name)
                            (not (constantp name)))
                   (name)
                 "Invalid slot name: ~s" slot-spec)
               (when (member name names :test #'eq)
                 (error "Duplicate slot names in struct"))
               (let ((type (parse-typespec type)))
                 (destructuring-bind
                     (&key (align (compute-alignment type) align-p)
                      (size (compute-fixed-size type))
                      (initform (when named (expand-prototype type)) initform-p))
                     rest
                   (check-type size non-negative-fixnum)
                   (check-type align non-negative-fixnum)
                   (when (and (not named) initform-p)
                     (error "Unnamed unions don't support :initforms for slots"))
                   (setf total-size (max total-size size)
                         total-align (max align total-align))                   
                   (list name type 0 align size initform))))
    :into slots
    :finally (return (values slots
                             (align-offset total-size total-align)
                             total-size
                             (or align-supplied total-align)))))

(define-type-parser union (options &rest slots)
  (destructuring-bind
      (&key (align nil align-p)) options
    (when align-p
      (check-type align positive-fixnum))
    (multiple-value-bind
        (slots size size-no-padding align)
        (parse-union-slots nil slots nil align)
      (make-instance 'union-type
        :slots slots
        :size size
        :size-no-padding size-no-padding
        :align align))))

(defmethod unparse-type ((type union-type))
  `(union (:align ,(struct-align type))
          ,@(loop :for (name slot-type offset align)
              :in (struct-slots type)
              :collect `(,name ,(unparse-type slot-type)                               
                               :align ,align))))

(defun notice-union-definition
    (name ctor-name conc-name include align slots)
  (setf (gethash name *union-type-hash*)
        (let ((included (when include
                          (or (gethash include *union-type-hash*)
                              (error "Undefined union type: ~s"
                                     include)))))
          (multiple-value-bind
              (slots size size-no-padding align)
              (parse-union-slots included slots name align)
            (make-instance 'named-union-type
              :name name
              :ctor-name ctor-name
              :conc-name conc-name
              :size size
              :include included
              :size-no-padding size-no-padding
              :align align
              :slots slots)))))

(defmacro define-union (name-and-options &rest slots)
  (let* ((name-and-options (ensure-list name-and-options))
         (name (first name-and-options))
         (ctor-name (gensym "CONSTRUCTOR")))
    (assert (and (symbolp name)
                 (not (constantp name)))
        (name))
    (destructuring-bind
        (&key constructor
         (copier nil copier-p)
         (conc-name (format nil "~a-" name))
         (type nil type-p)
         (align nil align-p)
         (include nil include-p)
         (predicate (intern (format nil "~a-~a" name 'p)) predicate-p)
         (print-object nil print-object-p)
         (print-function nil print-function-p))
        (flatten-options (rest name-and-options))
      (when align-p (assert (typep align 'non-negative-fixnum) (align)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (notice-union-definition
           ',name ',ctor-name ',conc-name ',(when include-p include)
           ',(when align-p align) ',slots)
         (declaim (inline ,ctor-name))
         (defstruct (,name
                      (:conc-name ,conc-name)
                      (:constructor ,ctor-name)
                      (:constructor ,@(ensure-list constructor))
                      ,@(when copier-p
                          `((:copier ,copier)))
                      ,@(when include-p
                          `((:include ,include)))
                      ,@(when predicate-p
                          `((:predicate ,predicate)))
                      ,@(when print-object-p
                          `((:print-object ,print-object)))
                      ,@(when print-function-p
                          `((:print-function ,print-function)))
                      ,@(when type-p
                          `((:type ,type))))
           ,@(slots->defstruct-slots slots))
         ,@(when type-p
             `((deftype ,name ()
                 ',(if (or (eq type 'vector)
                           (and (consp type)
                                (eq (car type) 'vector)))
                     (destructuring-bind
                         (&optional (elt-type t))
                         (rest (ensure-list type))
                       `(simple-array ,elt-type (*)))
                     type))
               (defun ,predicate (object)
                 (typep object ',name))))
         (define-type-parser ,name ()
           (gethash ',name *union-type-hash*))
         ',name))))
