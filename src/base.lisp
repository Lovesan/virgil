;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2010-2011, Dmitry Ignatiev <lovesan.ru@gmail.com>

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

(defvar *type-parsers* (make-hash-table :test #'eq))

(defun parse-typespec (typespec)
  (let* ((name (etypecase typespec
                 (symbol typespec)
                 (cons (let ((x (car typespec)))
                         (if (symbolp x)
                           x
                           (error "Invalid typespec: ~s" typespec))))))
         (args (if (symbolp typespec)
                 '()
                 (cdr typespec))))
    (multiple-value-bind
        (parser found-p) (gethash name *type-parsers*)
      (if found-p
        (apply parser args)
        (error "Undefined type: ~s" name)))))

(defclass translatable-type ()
  ())

(defun translatable-type-p (type)
  (typep type 'translatable-type))

(defmethod make-load-form ((type translatable-type) &optional env)
  (declare (ignore env))
  `(parse-typespec ',(unparse-type type)))

(defclass immediate-type (translatable-type)
  ())

(defun immediate-type-p (type)
  (typep type 'immediate-type))

(defclass primitive-type (immediate-type)
  ((name :initarg :name
         :reader primitive-type-name)
   (cffi-type :initarg :cffi-type
              :reader primitive-type-cffi-type)
   (lisp-type :initarg :lisp-type
              :reader primitive-type-lisp-type)
   (prototype :initarg :prototype
              :reader primitive-type-prototype)
   (prototype-expansion :initarg :prototype-expansion
                        :reader primitive-type-prototype-expansion)))

(defmethod print-object ((object primitive-type) stream)
  (print-unreadable-object
    (object stream :type t)
    (write (primitive-type-name object) :stream stream)))

(defun primitive-type-p (type)
  (typep type 'primitive-type))

(defun error-not-translatable-type (type)
  (error "~s is not a valid translatable type"
         type))


(defgeneric base-type (type)
  (:method ((type primitive-type))
    type)
  (:method ((type immediate-type))
    (parse-typespec 'int))
  (:method ((type translatable-type))
    (error "Unable to compute base type for type ~s"
           (unparse-type type)))
  (:method (type)
    (error-not-translatable-type type)))

(defgeneric unparse-type (type)
  (:method ((type primitive-type))
    (primitive-type-name type))
  (:method ((type translatable-type))
    (error "Unable to unparse type ~s" type))
  (:method (type)
    (error-not-translatable-type type)))

(defgeneric compute-fixed-size (type)
  (:method ((type primitive-type))
    (foreign-type-size (primitive-type-cffi-type type)))
  (:method ((type immediate-type))
    (compute-fixed-size (base-type type)))
  (:method ((type translatable-type))
    (error "Unable to compute fixed size for type ~s"
           (unparse-type type)))
  (:method (type)
    (error-not-translatable-type type)))

(defgeneric compute-size (value type)
  (:method (value (type primitive-type))
    (foreign-type-size (primitive-type-cffi-type type)))
  (:method (value (type immediate-type))
    (compute-size value (base-type type)))
  (:method (value (type translatable-type))
    (error "Unable to compute size of ~s with type of ~s"
           value (unparse-type type)))
  (:method (value type)
    (error-not-translatable-type type)))

(defgeneric expand-compute-size (value type)
  (:method (value (type primitive-type))
    (compute-fixed-size type))
  (:method (value (type immediate-type))
    (compute-fixed-size type))
  (:method (value (type translatable-type))
    `(compute-size ,value ,type))
  (:method (value type)
    (error-not-translatable-type type)))

(defgeneric compute-alignment (type)
  (:method ((type primitive-type))
    (foreign-type-alignment (primitive-type-cffi-type type)))
  (:method ((type immediate-type))
    (compute-alignment (base-type type)))
  (:method ((type translatable-type))
    (error "Unable to compute alignment of type ~s"
           (unparse-type type)))
  (:method (type)
    (error-not-translatable-type type)))

(defgeneric compute-slot-offset (slot type)
  (:method (slot (type translatable-type))
    (error "Unable to compute offset of slot ~s for type ~s"
           slot (unparse-type type)))
  (:method (slot type)
    (error-not-translatable-type type)))

(defgeneric expand-compute-slot-offset (slot type)
  (:method (slot-form (type translatable-type))
    `(compute-slot-offset ,slot-form ,type))
  (:method (slot-form type)
   (error-not-translatable-type type)))

(defgeneric prototype (type)
  (:method ((type primitive-type))
    (primitive-type-prototype type))
  (:method ((type immediate-type))
    (prototype (base-type type)))
  (:method ((type translatable-type))
    (error "Unable to compute prototype of type ~s"
           (unparse-type type)))
  (:method (type)
    (error-not-translatable-type type)))

(defgeneric expand-prototype (type)
  (:method ((type primitive-type))
    (primitive-type-prototype-expansion type))
  (:method ((type translatable-type))
    `(prototype ,type))
  (:method (type)
    (error-not-translatable-type type)))

(defgeneric lisp-type (type)
  (:method ((type primitive-type))
    (primitive-type-lisp-type type))
  (:method ((type immediate-type))
    (lisp-type (base-type type)))
  (:method ((type translatable-type))
    T)
  (:method (type)
   (error-not-translatable-type type)))

(defgeneric convert-value (lisp-value type)
  (:method (lisp-value (type translatable-type))
    lisp-value)
  (:method (lisp-value (type primitive-type))
    lisp-value)
  (:method (lisp-value (type immediate-type))
    (convert-value lisp-value (base-type type)))
  (:method (lisp-value type)
    (error-not-translatable-type type)))

(defgeneric translate-value (raw-value type)
  (:method (raw-value (type immediate-type))
    (translate-value raw-value (base-type type)))
  (:method (raw-value (type primitive-type))
    raw-value)
  (:method (raw-value (type translatable-type))
    raw-value)
  (:method (raw-value type)
    (error-not-translatable-type type)))

(defgeneric read-value (pointer out type)
  (:method (pointer out (type primitive-type))
    (declare (ignore out))
    (mem-ref pointer (primitive-type-cffi-type type)))
  (:method (pointer out (type immediate-type))
    (translate-value
      (read-value pointer out (base-type type))
      type))
  (:method (pointer out (type translatable-type))
    (error "Unable to read value of type ~s"
           (unparse-type type)))
  (:method (pointer out type)
    (error-not-translatable-type type)))

(defgeneric write-value (value pointer type)
  (:method (value pointer (type primitive-type))
    (setf (mem-ref pointer (primitive-type-cffi-type type)) value))
  (:method (value pointer (type immediate-type))
    (write-value (convert-value value type)
                 pointer
                 (base-type type)))
  (:method (value pointer (type translatable-type))
    (error "Unable to write value of type ~s"
           (unparse-type type)))
  (:method (value pointer type)
    (error-not-translatable-type type)))

(defgeneric expand-translate-value (raw-value type)
  (:method (raw-value (type translatable-type))
    `(translate-value ,raw-value ,type))
  (:method (raw-value (type primitive-type))
    raw-value)
  (:method (raw-value type)
    (error-not-translatable-type type)))

(defgeneric expand-convert-value (lisp-value type)
  (:method (lisp-value (type translatable-type))
    `(convert-value ,lisp-value ,type))
  (:method (lisp-value (type primitive-type))
    lisp-value)
  (:method (lisp-value type)
    (error-not-translatable-type type)))

(defgeneric expand-read-value (pointer out type)
  (:method (pointer out (type translatable-type))
    `(read-value ,pointer ,out ,type))
  (:method (pointer out (type primitive-type))
    (declare (ignore out))
    `(mem-ref ,pointer ',(primitive-type-cffi-type type)))
  (:method (pointer out (type immediate-type))
    (expand-translate-value
      (expand-read-value pointer out (base-type type))
      type))
  (:method (pointer out type)
    (error-not-translatable-type type)))

(defgeneric expand-write-value (value pointer type)
  (:method (value pointer (type translatable-type))
    `(write-value ,value ,pointer ,type))
  (:method (value pointer (type primitive-type))
    `(setf (mem-ref ,pointer ',(primitive-type-cffi-type type)) ,value))
  (:method (value pointer (type immediate-type))
    (expand-write-value (expand-convert-value value type)
                        pointer
                        (base-type type)))
  (:method (value pointer type)
    (error-not-translatable-type type)))

(defgeneric expand-dynamic-extent (var value-var body type)
  (:method (var value-var body type)
    `(let ((,var ,(expand-convert-value value-var type)))
       ,@body)))

(defgeneric expand-callback-dynamic-extent (var raw-value body type)
  (:method (var raw-value body (type translatable-type))
    `(let ((,var ,(expand-translate-value raw-value type)))
       (declare (type ,(lisp-type type) ,var))
       ,@body))
  (:method (var raw-value body type)
    (error-not-translatable-type type)))

(defgeneric allocate-value (value type)
  (:method (value (type translatable-type))
    (funcall 'raw-alloc (compute-size value type)))
  (:method (value (type primitive-type))
    (funcall 'raw-alloc (foreign-type-size (primitive-type-cffi-type type))))
  (:method (value type)
    (error-not-translatable-type type)))

(defgeneric expand-allocate-value (value-form type)
  (:method (value-form (type translatable-type))
    `(allocate-value ,value-form ,type))
  (:method (value-form (type primitive-type))
    `(raw-alloc ,(foreign-type-size (primitive-type-cffi-type type))))
  (:method (value-form type)
    (error-not-translatable-type type)))

(defgeneric clean-value (pointer value type)
  (:method (pointer value (type translatable-type))
    nil)
  (:method (pointer value type)
    (error-not-translatable-type type)))

(defgeneric expand-clean-value (pointer value type)
  (:method (pointer value (type primitive-type))
    nil)
  (:method (pointer value (type translatable-type))
    `(clean-value ,pointer ,value ,type))
  (:method (pointer value type)
    (error-not-translatable-type type)))

(defgeneric free-value (pointer type)
  (:method (pointer (type translatable-type))
    (declare (type foreign-pointer pointer))
    (funcall 'raw-free pointer)
    nil)
  (:method (pointer type)
    (error-not-translatable-type type)))

(defgeneric expand-free-value (pointer-form type)
  (:method (pointer-form (type translatable-type))
    `(free-value ,pointer-form ,type))
  (:method (pointer-form (type primitive-type))
    `(progn 
       (raw-free (the foreign-pointer ,pointer-form))
       nil))
  (:method (pointer-form type)
    (error-not-translatable-type type)))

(defmacro %unwind-protect (form &rest cleanup-forms)
  (if (find (complement #'constantp) cleanup-forms)
    `(unwind-protect
         ,form
       ,@cleanup-forms)
    form))

(defgeneric expand-reference-dynamic-extent
    (var size-var value-var body mode type)
  (:method (var size-var value-var body mode (type translatable-type))
    (with-gensyms (pointer-var)
      `(with-raw-pointer (,pointer-var ,(eval-if-constantp
                                          (expand-compute-size value-var type))
                                       ,size-var)
         (let ((,var ,pointer-var))           
           (%unwind-protect
             ,(ecase mode
                (:in `(progn ,(expand-write-value value-var pointer-var type)
                             nil
                             ,@body))
                (:out `(prog1 (progn ,@body)
                        (setf ,value-var
                              ,(expand-read-value pointer-var value-var type))))
                (:inout `(progn ,(expand-write-value value-var pointer-var type)
                                (prog1 (progn ,@body)
                                 (setf ,value-var
                                       ,(expand-read-value
                                          pointer-var value-var type))))))
             (progn               
               ,(expand-clean-value pointer-var value-var type)))))))
  (:method (var size-var value-var body mode type)
    (error-not-translatable-type type)))

(defun eval-if-constantp (x)
  (if (constantp x)
    (values (eval x) T)
    (values x nil)))


(defvar *handle-cycles* nil)
(defvar *written-values*)
(defvar *readen-values*)
(defvar *cleaned-values*)

(defun enable-circular-references ()
  (setf *handle-cycles* t
        *written-values* '()
        *readen-values* '()
        *cleaned-values* '())
  nil)

(defun disable-circular-references ()
  (setf *handle-cycles* nil)
  (makunbound '*written-values*)
  (makunbound '*readen-values*)
  (makunbound '*cleaned-values*)
  nil)

(defun clear-circular-reference-cache ()
  (when *handle-cycles*
    (setf *written-values* '()
          *readen-values* '()
          *cleaned-values* '()))
  nil)

(defmacro with-circular-references (&body body)
  `(progv '(*written-values*
            *readen-values*
            *cleaned-values*)
          '(() () () () ())
     (let ((*handle-cycles* T))
       ,@body)))

(defmacro without-circular-references (&body body)
  `(progv '(*written-values*
            *readen-values*
            *cleaned-values*)
          '()
     (let ((*handle-cycles* nil))
       ,@body)))

(defun alloc (type &optional (value nil value-p))
  (let* ((type (parse-typespec type))
         (value (if value-p value (prototype type)))
         (pointer (allocate-value value type)))
    (declare (type pointer pointer))
    (write-value value pointer type)
    pointer))

(define-compiler-macro alloc (&whole form type-form
                              &optional (value-form nil value-p))
  (multiple-value-bind
      (type-form constantp) (eval-if-constantp type-form)
    (if constantp
      (let ((type (parse-typespec type-form)))
        (with-gensyms (value pointer)
          `(progn
             (let* ((,value ,(if value-p
                               value-form
                               (expand-prototype type)))
                    (,pointer ,(expand-allocate-value value type)))
               (declare (type pointer ,pointer))
               ,(expand-write-value value pointer type)
               ,pointer))))
      form)))

(defun free (pointer &optional (type nil type-p))
  (declare (type pointer pointer))
  (if type-p
    (free-value pointer (parse-typespec type))
    (funcall 'raw-free pointer)))

(define-compiler-macro free (&whole form pointer &optional (type nil type-p))
  (if type-p
    (multiple-value-bind
        (type constantp) (eval-if-constantp type)    
      (if constantp
        `(progn
           ,(expand-free-value pointer (parse-typespec type)))
        form))
    `(raw-free ,pointer)))

(defun clean (pointer value type)
  (declare (type pointer pointer))
  (clean-value pointer value (parse-typespec type)))

(define-compiler-macro clean (&whole form pointer value type)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      `(progn
         ,(expand-clean-value pointer value (parse-typespec type)))
      form)))

(defun clean-and-free (pointer value type)
  (declare (type pointer pointer))
  (let ((type (parse-typespec type)))
    (clean-value pointer value type)
    (free-value pointer type)))

(define-compiler-macro clean-and-free (&whole form pointer value type)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      (once-only (pointer)
        (let ((type (parse-typespec type)))
          `(progn
             ,(expand-clean-value pointer value type)
             ,(expand-free-value pointer type))))
      form)))

(defun sizeof (type &optional (value nil value-p))
  (if value-p
    (compute-size value (parse-typespec type))
    (compute-fixed-size (parse-typespec type))))

(define-compiler-macro sizeof (type &optional (value nil value-p))
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      (if value-p
        (expand-compute-size value (parse-typespec type))
        (compute-fixed-size (parse-typespec type)))
      `(compute-size ,value (parse-typespec ,type)))))

(defun alignof (type)
  (compute-alignment (parse-typespec type)))

(define-compiler-macro alignof (type)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      (compute-alignment (parse-typespec type))
      `(compute-alignment (parse-typespec ,type)))))

(defun offsetof (type member)
  (compute-slot-offset member (parse-typespec type)))

(define-compiler-macro offsetof (type member)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      (expand-compute-slot-offset member (parse-typespec type))
      `(compute-slot-offset ,member (parse-typespec ,type)))))

(defun convert (lisp-value type)
  (convert-value lisp-value (parse-typespec type)))

(define-compiler-macro convert (lisp-value type)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    `(progn
       ,(if constantp
          (expand-convert-value lisp-value (parse-typespec type))
          `(convert-value ,lisp-value (parse-typespec ,type))))))

(defun translate (raw-value type)
  (translate-value raw-value (parse-typespec type)))

(define-compiler-macro translate (raw-value type)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
   `(progn
      ,(if constantp
         (expand-translate-value raw-value (parse-typespec type))
         `(translate-value ,raw-value (parse-typespec ,type))))))

(defun flatten-options (options)
  (loop :with parsed = '()
    :for opt :in options
    :if (or (not (listp opt))
            (not (keywordp (car opt))))
    :do   (error "Invalid option: ~s" opt)
    :else :if (find (car opt) parsed :test #'eq)
    :do (error "Duplicate option: ~s" opt)
    :else
    :collect (car opt)
    :collect (if (> (length opt) 2)
               (cdr opt)
               (cadr opt))))


(defmacro %dotimes ((iter limit &optional retval)
                    &body body)
  (if (and (every #'constantp body))
    `(let ((,iter ,limit))
       (declare (ignorable ,iter))
       ,retval)
    `(dotimes (,iter ,limit ,retval)
       ,@body)))
