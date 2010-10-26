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

(defun valid-function-name-p (name)
  (or (symbolp name)
      (and (proper-list-p name)
           (= 2 (length name))
           (eq 'setf (first name))
           (symbolp (second name)))))

(define-translatable-type struct-type ()
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
          :reader struct-align)
   (allocator :initform nil
              :initarg :allocator
              :reader struct-allocator)
   (reader :initform nil
           :initarg :reader
           :reader struct-reader)
   (writer :initform nil
           :initarg :writer
           :reader struct-writer)
   (cleaner :initform nil
            :initarg :cleaner
            :reader struct-cleaner)
   (deallocator :initform nil
                :initarg :deallocator
                :reader struct-deallocator))
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
    (if (struct-reader type)
      (funcall (fdefinition (struct-reader type)) pointer out)
      (let* ((result (or out (prototype type))))
        (declare (type hash-table result))
        (loop :for (name slot-type offset) :in (struct-slots type)
          :do (setf (gethash name result)
                    (read-value (&+ pointer offset)
                                (gethash name result)
                                slot-type)))
        result)))
  (:reader-expansion (pointer out type)
    (if (struct-reader type)
      `(funcall #',(struct-reader type) ,pointer ,out)
      (with-gensyms (result)
        (once-only (pointer out)
          `(let ((,result (or ,out ,(expand-prototype type))))
             (declare (type hash-table ,result)
                      (type pointer ,pointer))
             ,@(loop :for (name slot-type offset) :in (struct-slots type)
                 :collect `(setf (gethash ',name ,result)
                                 ,(expand-read-value
                                    `(&+ ,pointer ,offset)
                                    `(gethash ',name ,result)
                                    slot-type)))
             ,result)))))
  (:writer (value pointer type)
    (if (struct-writer type)
      (funcall (fdefinition (struct-writer type)) value pointer)
      (loop :for (name slot-type offset) :in (struct-slots type)
        :do (write-value (gethash name value)
                         (&+ pointer offset)
                         slot-type)
        :finally (return value))))
  (:writer-expansion (value-form pointer-form type)
    (if (struct-writer type)
      `(funcall #',(struct-writer type) ,value-form ,pointer-form)
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
           ,value))))
  (:cleaner (pointer value type)
    (if (struct-cleaner type)
      (funcall (fdefinition (struct-cleaner type)) pointer value)
      (loop :for (name slot-type offset) :in (struct-slots type)
        :do (clean-value (&+ pointer offset)
                         (gethash name value)
                         slot-type))))
  (:cleaner-expansion (pointer-form value-form type)
    (if (struct-cleaner type)
      `(funcall #',(struct-cleaner type) ,pointer-form ,value-form)
      (with-gensyms (pointer value)
        `(let ((,pointer ,pointer-form) (,value ,value-form))
           (declare (type pointer ,pointer)
                    (type hash-table ,value)
                    (ignorable ,pointer ,value))
           ,@(loop :for (name slot-type offset) :in (struct-slots type)
               :collect (expand-clean-value
                          `(&+ ,pointer ,offset)
                          `(gethash ',name ,value)
                          slot-type))))))
  (:allocator (value type)
    (if (struct-allocator type)
      (funcall (fdefinition (struct-allocator type)) value)
      (raw-alloc (compute-fixed-size type))))
  (:allocator-expansion (value type)
    (if (struct-allocator type)
      `(funcall #',(struct-allocator type) ,value)
      `(raw-alloc ,(compute-fixed-size type))))
  (:deallocator (pointer type)
    (if (struct-deallocator type)
      (funcall (fdefinition (struct-deallocator type)) pointer)
      (raw-free pointer)))
  (:deallocator-expansion (pointer type)
    (if (struct-deallocator type)
      `(funcall #',(struct-deallocator type) ,pointer)
      `(raw-free ,pointer)))
  (:slot-offset (member-name type)
    (check-type member-name symbol)
    (loop :for (slot-name slot-type slot-offset)
      :in (struct-slots type)
      :when (eq slot-name member-name) :do (return slot-offset)
      :finally (error "Structure type ~s has no slot named ~s"
                      (unparse-type type)
                      member-name)))
  (:slot-offset-expansion (member-name type)
    (multiple-value-bind
        (member-name constantp) (eval-if-constantp member-name)
      (if constantp
        (let ((slot (progn
                      (check-type member-name symbol)
                      (find member-name (struct-slots type)
                            :key #'first :test #'eq))))
          (if (null slot)          
            (error "Structure type ~s has no slot named ~s"
                   (unparse-type type)
                   member-name)
            (third slot)))
        (once-only (member-name)
          `(progn
             (check-type ,member-name symbol)
             (case ,member-name
               ,@(loop :for (slot-name slot-type slot-offset)
                   :in (struct-slots type)
                   :collect `(,slot-name ,slot-offset))
               (T (error "Structure type ~s has no slot named ~s"
                         ',(unparse-type type)
                         ,member-name)))))))))

(defvar *readen-struct-types* '())
(defvar *written-struct-types* '())
(defvar *cleaned-struct-types* '())

(define-translatable-type named-struct-type (struct-type)
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
    (if (struct-reader type)
      (funcall (fdefinition (struct-reader type)) pointer out)
      (let* ((result (or out (prototype type)))
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
        result)))
  (:writer (value pointer type)
    (if (struct-writer type)
      (funcall (fdefinition (struct-writer type)) value pointer)
      (progn
        (when (struct-included type)
          (write-value value pointer (struct-included type)))
        (loop :with conc-name = (struct-conc-name type)
          :for (slot-name slot-type offset) :in (struct-slots type)
          :for accessor = (intern (format nil "~a~a" conc-name slot-name)
                                  (struct-package type))
          :do (write-value (funcall accessor value)
                           (&+ pointer offset)
                           slot-type))
        value)))
  (:cleaner (pointer value type)
    (if (struct-cleaner type)
      (funcall (fdefinition (struct-cleaner type)) pointer value)
      (progn
        (when (struct-included type)
          (clean-value pointer value (struct-included type)))
        (loop :with conc-name = (struct-conc-name type)
          :for (slot-name slot-type offset) :in (struct-slots type)
          :for accessor = (intern (format nil "~a~a" conc-name slot-name)
                                  (struct-package type))
          :do (clean-value (&+ pointer offset)
                           (funcall accessor value)
                           slot-type))))))

(defun make-reader-name (type)
  (intern (format nil "~a::~a::~a"
                  (package-name *package*)
                  (struct-name type)
                  'reader)
          :virgil))

(defun make-writer-name (type)
  (intern (format nil "~a::~a::~a"
                  (package-name *package*)
                  (struct-name type)
                  'writer)
          :virgil))

(defun make-cleaner-name (type)
  (intern (format nil "~a::~a::~a"
                  (package-name *package*)
                  (struct-name type)
                  'cleaner)
          :virgil))

(defmethod expand-read-value (pointer-form out-form (type named-struct-type))
  (if (struct-reader type)
    `(funcall #',(struct-reader type) ,pointer-form ,out-form)
    (let ((reader (make-reader-name type)))
      (if (find (struct-name type) *readen-struct-types*)
        `(,reader ,pointer-form ,out-form)
        (let ((*readen-struct-types* (cons (struct-name type)
                                           *readen-struct-types*)))
          (with-gensyms (result pointer out)
            `(labels ((,reader (,pointer ,out)
                        (declare (type pointer ,pointer))
                        (let* ((,result (or ,out ,(expand-prototype type))))
                          (declare (type ,(lisp-type type) ,result))
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
                          ,result)))
               (,reader ,pointer-form ,out-form))))))))

(defmethod expand-write-value (value-form pointer-form (type named-struct-type))
  (if (struct-writer type)
    `(funcall #',(struct-writer type) ,value-form ,pointer-form)
    (let ((writer (make-writer-name type)))
      (if (find (struct-name type) *written-struct-types*)
        `(,writer ,value-form ,pointer-form)
        (let ((*written-struct-types* (cons (struct-name type)
                                            *written-struct-types*)))
          (with-gensyms (pointer value)
            `(labels ((,writer (,value ,pointer)
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
                        ,value))
               (,writer ,value-form ,pointer-form))))))))

(defmethod expand-clean-value (pointer-form value-form (type named-struct-type))
  (if (struct-cleaner type)
    `(funcall #',(struct-cleaner type) ,pointer-form ,value-form)
    (let ((cleaner (make-cleaner-name type)))
      (if (find (struct-name type) *cleaned-struct-types*)
        `(,cleaner ,pointer-form ,value-form)
        (let ((*cleaned-struct-types* (cons (struct-name type)
                                            *cleaned-struct-types*)))
          (with-gensyms (pointer value)
            `(labels ((,cleaner (,pointer ,value)
                        (declare (type pointer ,pointer)
                                 (type ,(lisp-type type) ,value)
                                 (ignorable ,pointer ,value))                        
                        ,(when (struct-included type)
                           (expand-clean-value pointer value (struct-included type)))
                        ,@(loop :with conc-name = (struct-conc-name type)
                            :for (slot-name slot-type offset) :in (struct-slots type)
                            :for accessor = (intern (format nil "~a~a" conc-name slot-name)
                                                    (struct-package type))
                            :collect (expand-clean-value
                                       `(&+ ,pointer ,offset)
                                       `(,accessor ,value)
                                       slot-type))))
               (,cleaner ,pointer-form ,value-form))))))))
  
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
               (when (eq type named)
                 (error "Recursive structure definition: ~s" slot-spec))
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
      (&key align packed allocator deallocator reader writer cleaner)
      options
    (check-type align (or null positive-fixnum))
    (assert (valid-function-name-p allocator) (allocator))
    (assert (valid-function-name-p deallocator) (deallocator))
    (assert (valid-function-name-p reader) (reader))
    (assert (valid-function-name-p writer) (writer))
    (assert (valid-function-name-p cleaner) (cleaner))
    (multiple-value-bind
        (slots size size-no-padding align)
        (parse-struct-slots nil slots nil align packed)
      (make-instance 'struct-type
        :slots slots
        :size size
        :size-no-padding size-no-padding
        :align align
        :allocator (if (eq allocator :default) nil allocator)
        :deallocator (if (eq deallocator :default) nil deallocator)
        :reader (if (eq reader :default) nil reader)
        :writer (if (eq writer :default) nil writer)
        :cleaner (if (eq cleaner :default) nil cleaner)))))

(defmethod unparse-type ((type struct-type))
  `(struct (:align ,(struct-align type)
            :allocator ,(or (struct-allocator type) :default)
            :deallocator ,(or (struct-deallocator type) :default)
            :reader ,(or (struct-reader type) :default)
            :writer ,(or (struct-writer type) :default)
            :cleaner ,(or (struct-cleaner type) :default))
           ,@(loop :for (name slot-type offset align)
               :in (struct-slots type)
               :collect `(,name ,(unparse-type slot-type)
                                :offset ,offset
                                :align ,align))))

(defmethod unparse-type ((type named-struct-type))
  (struct-name type))

(defun notice-struct-definition (name ctor-name options slots)
  (let ((include (getf options :include))
        (align (getf options :align))
        (packed (getf options :packed)))
    (let* ((included (when include
                       (or (gethash include *struct-type-hash*)
                           (error "Undefined structure type: ~s"
                                  include))))
           (type (setf (gethash name *struct-type-hash*)
                       (make-instance 'named-struct-type
                         :name name
                         :include included
                         :ctor-name ctor-name
                         :conc-name (getf options :conc-name)
                         :allocator (getf options :allocator)
                         :deallocator (getf options :deallocator)
                         :reader (getf options :reader)
                         :writer (getf options :writer)
                         :cleaner (getf options :cleaner)))))
      (multiple-value-bind
          (slots size size-no-padding align)
          (parse-struct-slots included slots name align packed)
        (setf (slot-value type 'size-no-padding) size-no-padding              
              (slot-value type 'size) size
              (slot-value type 'align) align
              (slot-value type 'slots) slots))))
  nil)
  
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

(defun assert-not-duplicate-option (option-name options unparsed)
  (when (getf options option-name)
    (error "Duplicate ~s option: ~s" option-name unparsed)))

(defun assert-symbolp-option-name (option-name name rest)
  (unless (symbolp name)
    (error "~s name is not a symbol: ~s"
           option-name (cons option-name rest))))

(defun parse-struct-options (name unparsed)
  (let ((options '()))
    (loop :for (oname . rest) :in unparsed
      :do (case oname
            (:conc-name (destructuring-bind
                            (&optional name) rest
                          (unless name (setf name ""))
                          (assert-not-duplicate-option :conc-name options unparsed)
                          (setf (getf options :conc-name) name)))
            (:constructor (destructuring-bind
                              (name &optional (args nil args-p)) rest
                            (assert-symbolp-option-name :constructor name rest)
                            (unless (listp args)
                              (error "Invalid constructor lambda-list: ~s"
                                     (cons name rest)))
                            (push (cons name (if args-p (list args) '()))
                                  (getf options :constructors))))
            (:allocator (destructuring-bind
                            (allocator-name) rest                          
                          (assert (valid-function-name-p allocator-name)
                              (allocator-name)
                            "Invalid function name: ~s" allocator-name)
                          (assert-not-duplicate-option :allocator options unparsed)
                          (setf (getf options :allocator)
                                (if (eq allocator-name :default)
                                  nil
                                  allocator-name))))
            (:deallocator (destructuring-bind
                              (deallocator-name) rest
                            (assert (valid-function-name-p deallocator-name)
                              (deallocator-name)
                            "Invalid function name: ~s" deallocator-name)
                            (assert-not-duplicate-option :deallocator options unparsed)
                            (setf (getf options :deallocator)
                                  (if (eq deallocator-name :default)
                                    nil
                                    deallocator-name))))
            (:reader (destructuring-bind
                         (reader-name) rest
                       (assert (valid-function-name-p reader-name)
                              (reader-name)
                            "Invalid function name: ~s" reader-name)
                       (assert-not-duplicate-option :reader options unparsed)
                       (setf (getf options :reader)
                             (if (eq reader-name :default)
                               nil
                               reader-name))))
            (:writer (destructuring-bind
                         (writer-name) rest
                       (assert (valid-function-name-p writer-name)
                              (writer-name)
                            "Invalid function name: ~s" writer-name)
                       (assert-not-duplicate-option :writer options unparsed)
                       (setf (getf options :writer)
                             (if (eq writer-name :default)
                               nil
                               writer-name))))
            (:cleaner (destructuring-bind
                          (cleaner-name) rest
                        (assert (valid-function-name-p cleaner-name)
                            (cleaner-name)
                          "Invalid function name: ~s" cleaner-name)
                        (assert-not-duplicate-option :cleaner options unparsed)
                        (setf (getf options :cleaner)
                              (if (eq cleaner-name :default)
                                nil
                                cleaner-name))))
            (:type (destructuring-bind
                       (name) rest
                     (assert-not-duplicate-option :type options unparsed)
                     (setf (getf options :type) name)))
            (:copier (destructuring-bind
                         (name) rest
                       (assert-symbolp-option-name :copier name rest)
                       (assert-not-duplicate-option :copier options unparsed)
                       (setf (getf options :copier) name)))
            (:align (destructuring-bind
                        (align) rest
                      (check-type align non-negative-fixnum)
                      (assert-not-duplicate-option :align options unparsed)
                      (setf (getf options :align) align)))
            (:packed (destructuring-bind
                         (packed) rest
                       (check-type packed boolean)
                       (assert-not-duplicate-option :packed options unparsed)
                       (setf (getf options :packed) packed)))
            (:include (destructuring-bind
                          (include) rest
                        (check-type include symbol)
                        (assert-not-duplicate-option :include options unparsed)
                        (setf (getf options :include) include)))
            (:predicate (destructuring-bind
                            (name) rest
                          (assert-symbolp-option-name :predicate name rest)
                          (assert-not-duplicate-option :predicate options unparsed)
                          (setf (getf options :predicate) name)))
            (:print-object (destructuring-bind
                               (printer) rest
                             (assert-not-duplicate-option :print-object
                               options unparsed)
                             (setf (getf options :print-object) printer)))
            (:print-function (destructuring-bind
                                 (printer) rest
                               (assert-not-duplicate-option :print-function
                                 options unparsed)
                               (setf (getf options :print-function) printer)))
            (T (error "Undefined option: ~s" (cons oname rest)))))
    (let ((nokey (gensym)))
      (when (eq nokey (getf options :constructors nokey))
        (push (list (intern (format nil "~a-~a" 'make name)))
              (getf options :constructors)))
      (when (eq nokey (getf options :conc-name nokey))
        (setf (getf options :conc-name)
              (format nil "~a-" name)))
      (when (eq nokey (getf options :predicate nokey))
        (setf (getf options :predicate)
              (intern (format nil "~a-~a" name 'p)))))
    options))

(defmacro %define-struct (name ctor-name options &body slots)
  (destructuring-bind
      (&key conc-name constructors (copier nil copier-p)
       predicate (type nil type-p) (print-object nil print-object-p)
       (print-function nil print-function-p)
       (include nil include-p) &allow-other-keys)
      options
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defstruct (,name
                    (:conc-name ,conc-name)
                    (:constructor ,ctor-name)
                    ,@(mapcar (lambda (x) (cons :constructor x))
                        constructors)
                    ,@(when copier-p `((:copier ,copier)))
                    ,@(when include-p `((:include ,include)))
                    ,@(unless type-p `((:predicate ,predicate)))
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
               (typep object ',name)))))))
  
(defun make-internal-ctor-name (struct-name)
  (intern (format nil "~a::~a::~a"
                  (package-name *package*)
                  struct-name
                  'private-constructor)
          :virgil))
  
(defmacro define-struct (name-and-options &rest slots)
  (let* ((name-and-options (ensure-list name-and-options))
         (name (first name-and-options))
         (ctor-name (make-internal-ctor-name name))
         (options (parse-struct-options name (rest name-and-options))))
    (check-type name symbol)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (define-type-parser ,name ()
           (gethash ',name *struct-type-hash*))
         (notice-struct-definition ',name ',ctor-name ',options ',slots))
       (declaim (inline ,ctor-name))
       (%define-struct ,name ,ctor-name ,options ,@slots)
       ',name)))

(defun union-default-type (type)
  (lastcar (ensure-list (lisp-type type))))

(define-translatable-type union-type (struct-type)
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
            (return (clean-value pointer value slot-type)))
      :finally (if (typep value (union-default-type type))
                 (call-next-method pointer value type)
                 (error "~s is invalid value for union type ~s"
                        value (unparse-type type)))))
  (:cleaner-expansion (pointer value type)
    (once-only (pointer value)
      `(etypecase ,value
         ,@(loop :for (name slot-type) :in (struct-slots type)
             :collect `(,(lisp-type slot-type)
                        ,(expand-clean-value
                           pointer
                           value
                           slot-type)))
         (,(union-default-type type) ,(call-next-method pointer value type))))))

;;Time for CLOS magic to come
(define-translatable-type named-union-type (union-type named-struct-type)
  ())

(define-immediate-type immediate-union-type (union-type)
  ((base-type :initarg :base-type
              :initform (error "Immediate union w/o base type")              
              :reader base-type))
  (:converter (value type)
    (with-raw-pointer (p (compute-fixed-size (base-type type)))
      (write-value value p type)
      (read-value p nil (base-type type))))
  (:converter-expansion (value type)
    (with-gensyms (pointer)
      `(with-raw-pointer (,pointer ,(compute-fixed-size (base-type type)))
         ,(expand-write-value value pointer type)
         ,(expand-read-value pointer nil (base-type type)))))
  (:translator (value type)
    (with-raw-pointer (p (compute-fixed-size (base-type type)))
      (write-value value p (base-type type))
      (read-value p nil type)))
  (:translator-expansion (value type)
    (with-gensyms (pointer)
      `(with-raw-pointer (,pointer ,(compute-fixed-size (base-type type)))
         ,(expand-write-value value pointer (base-type type))
         ,(expand-read-value pointer nil type))))
  (:dynamic-extent-expansion (var value body type)
    (with-gensyms (pointer)
      (once-only (value)
        `(with-raw-pointer (,pointer ,(compute-fixed-size (base-type type)))
           ,(expand-write-value value pointer type)
           (prog1 (let ((,var ,(expand-read-value pointer nil (base-type type))))
                    ,@body)
            ,(expand-clean-value pointer value type)))))))

(define-immediate-type immediate-named-union-type
    (immediate-union-type named-struct-type)
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
      (&key (align nil align-p) allocator deallocator reader writer cleaner)
      options
    (when align-p
      (check-type align positive-fixnum))
    (assert (valid-function-name-p allocator) (allocator))
    (assert (valid-function-name-p deallocator) (deallocator))
    (assert (valid-function-name-p reader) (reader))
    (assert (valid-function-name-p writer) (writer))
    (assert (valid-function-name-p cleaner) (cleaner))
    (multiple-value-bind
        (slots size size-no-padding align)
        (parse-union-slots nil slots nil align)
      (if (every (lambda (spec &aux (type (second spec)) (size (fifth spec)))
                   (and (or (immediate-type-p type)
                            (primitive-type-p type))
                        (member size '(1 2 4 8) :test #'=)))
                 slots)
        (make-instance 'immediate-union-type
          :allocator (if (eq allocator :default) nil allocator)
          :deallocator (if (eq deallocator :default) nil deallocator)
          :reader  (if (eq reader :default) nil reader)
          :writer  (if (eq writer :default) nil writer)
          :cleaner  (if (eq cleaner :default) nil cleaner)
          :slots slots
          :size size
          :size-no-padding size-no-padding
          :align align
          :base-type (parse-typespec
                       (ecase (reduce #'max (mapcar #'fifth slots))
                         (1 'uint8)
                         (2 'uint16)
                         (4 'uint32)
                         (8 'uint64))))
        (make-instance 'union-type
          :allocator (if (eq allocator :default) nil allocator)
          :deallocator (if (eq deallocator :default) nil deallocator)
          :reader  (if (eq reader :default) nil reader)
          :writer  (if (eq writer :default) nil writer)
          :cleaner  (if (eq cleaner :default) nil cleaner)
          :slots slots
          :size size
          :size-no-padding size-no-padding
          :align align)))))
  
(defmethod unparse-type ((type union-type))
  `(union (:align ,(struct-align type)
           :allocator ,(or (struct-allocator type) :default)
           :deallocator ,(or (struct-deallocator type) :default)
           :reader ,(or (struct-reader type) :default)
           :writer ,(or (struct-writer type) :default)
           :cleaner ,(or (struct-cleaner type) :default))
          ,@(loop :for (name slot-type offset align)
              :in (struct-slots type)
              :collect `(,name ,(unparse-type slot-type)                               
                               :align ,align))))

(defun notice-union-definition (name ctor-name options slots)
  (let ((conc-name (getf options :conc-name))
        (include (getf options :include))
        (align (getf options :align)))
    (setf (gethash name *union-type-hash*)
          (let ((included (when include
                            (or (gethash include *union-type-hash*)
                                (error "Undefined union type: ~s"
                                       include)))))
            (multiple-value-bind
                (slots size size-no-padding align)
                (parse-union-slots included slots name align)
              (if (every (lambda (spec &aux (type (second spec)))
                           (or (immediate-type-p type)
                               (primitive-type-p type)))
                         slots)
                (make-instance 'immediate-named-union-type
                  :allocator (getf options :allocator)
                  :deallocator (getf options :deallocator)
                  :reader (getf options :reader)
                  :writer (getf options :writer)
                  :cleaner (getf options :cleaner)
                  :name name
                  :ctor-name ctor-name
                  :conc-name conc-name
                  :include included
                  :slots slots
                  :size size
                  :size-no-padding size-no-padding
                  :align align
                  :base-type (loop :with max-type = (parse-typespec 'byte)
                               :with max-size = 1
                               :for (name type offs align size) :in slots
                               :when (> size max-size) :do (setf max-size size
                                                                 max-type type)
                               :finally (return max-type)))
                (make-instance 'named-union-type
                  :allocator (getf options :allocator)
                  :deallocator (getf options :deallocator)
                  :reader (getf options :reader)
                  :writer (getf options :writer)
                  :cleaner (getf options :cleaner)
                  :name name
                  :ctor-name ctor-name
                  :conc-name conc-name
                  :size size
                  :include included
                  :size-no-padding size-no-padding
                  :align align
                  :slots slots)))))))

(defmacro define-union (name-and-options &rest slots)
  (let* ((name-and-options (ensure-list name-and-options))
         (name (first name-and-options))
         (ctor-name (make-internal-ctor-name name))
         (options (parse-struct-options name (rest name-and-options))))
    (check-type name symbol)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (notice-union-definition ',name ',ctor-name ',options ',slots)
         (define-type-parser ,name ()
           (gethash ',name *union-type-hash*)))
       (declaim (inline ,ctor-name))
       (%define-struct ,name ,ctor-name ,options ,@slots)
       ',name)))
