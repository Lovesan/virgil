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

(defstruct primitive-type
  name
  cffi-type
  lisp-type
  prototype)

(defclass immediate-type ()
  ((base-type :initarg :base-type
              :initform (parse-typespec 'int)
              :reader base-type)))

(defclass aggregate-type ()
  ())

(defgeneric unparse-type (type)
  (:method ((type primitive-type))
    (primitive-type-name type))
  (:method (type)
    (error "Unable to unparse type ~s" type)))

(defgeneric compute-fixed-size (type)
  (:method ((type primitive-type))
    (foreign-type-size (primitive-type-cffi-type type)))
  (:method ((type immediate-type))
    (compute-fixed-size (base-type type)))
  (:method (type)
    (error "Unable to compute fixed size for type ~s"
           (unparse-type type))))

(defgeneric compute-size (value type)
  (:method (value (type primitive-type))
    (foreign-type-size (primitive-type-cffi-type type)))
  (:method (value (type immediate-type))
    (compute-size value (base-type type)))
  (:method (value type)
    (error "Unable to compute size of ~s with type of ~s"
           value (unparse-type type))))

(defgeneric expand-compute-size (value type)
  (:method (value (type primitive-type))
    (compute-fixed-size type))
  (:method (value (type immediate-type))
    (compute-fixed-size type))
  (:method (value type)
    `(compute-size ,value (parse-typespec ',(unparse-type type)))))

(defgeneric compute-alignment (type)
  (:method ((type primitive-type))
    (foreign-type-alignment (primitive-type-cffi-type type)))
  (:method ((type immediate-type))
    (compute-alignment (base-type type)))
  (:method (type)
    (error "Unable to compute alignment of type ~s"
           (unparse-type type))))

(defgeneric prototype (type)
  (:method ((type primitive-type))
    (primitive-type-prototype type))
  (:method ((type immediate-type))
    (prototype (base-type type)))
  (:method (type)
    (error "Unable to compute prototype of type ~s"
           (unparse-type type))))

(defgeneric expand-prototype (type)
  (:method ((type primitive-type))
    (primitive-type-prototype type))
  (:method (type)
    `(prototype (parse-typespec ',(unparse-type type)))))

(defgeneric lisp-type (type)
  (:method ((type primitive-type))
    (primitive-type-lisp-type type))
  (:method ((type immediate-type))
    (lisp-type (base-type type)))
  (:method (type) T))

(defgeneric convert-value (lisp-value type)
  (:method (lisp-value type)
    lisp-value)
  (:method (lisp-value (type immediate-type))
    (convert-value lisp-value (base-type type))))

(defgeneric translate-value (raw-value type)
  (:method (raw-value (type immediate-type))
    (translate-value raw-value (base-type type)))
  (:method (raw-value type)
    raw-value))

(defgeneric read-value (pointer out type)
  (:method (pointer out (type primitive-type))
    (declare (ignore out))
    (mem-ref pointer (primitive-type-cffi-type type)))
  (:method (pointer out (type immediate-type))
    (translate-value
      (read-value pointer out (base-type type))
      type))
  (:method (pointer out type)
    (error "Unable to read value of type ~s"
           (unparse-type type))))

(defgeneric write-value (value pointer type)
  (:method (value pointer (type primitive-type))
    (setf (mem-ref pointer (primitive-type-cffi-type type)) value))
  (:method (value pointer (type immediate-type))
    (write-value (convert-value value type)
                 pointer
                 (base-type type)))
  (:method (value pointer type)
    (error "Unable to write value of type ~s"
           (unparse-type type))))

(defgeneric expand-translate-value (raw-value type)
  (:method (raw-value type)
    `(translate-value ,raw-value (parse-typespec ',(unparse-type type))))
  (:method (raw-value (type primitive-type))
    raw-value))

(defgeneric expand-convert-value (lisp-value type)
  (:method (lisp-value type)
    `(convert-value ,lisp-value (parse-typespec ',(unparse-type type))))
  (:method (lisp-value (type primitive-type))
    lisp-value))

(defgeneric expand-read-value (pointer out type)
  (:method (pointer out type)
    `(read-value ,pointer ,out (parse-typespec ',(unparse-type type))))
  (:method (pointer out (type primitive-type))
    (declare (ignore out))
    `(mem-ref ,pointer ',(primitive-type-cffi-type type)))
  (:method (pointer out (type immediate-type))
    (expand-translate-value
      (expand-read-value pointer out (base-type type))
      type)))

(defgeneric expand-write-value (value pointer type)
  (:method (value pointer type)
    `(write-value ,value ,pointer (parse-typespec ',(unparse-type type))))
  (:method (value pointer (type primitive-type))
    `(setf (mem-ref ,pointer ',(primitive-type-cffi-type type)) ,value))
  (:method (value pointer (type immediate-type))
    (expand-write-value (expand-convert-value value type)
                        pointer
                        (base-type type))))

(defgeneric expand-dynamic-extent (var value-var body type)
  (:method (var value-var body type)
    `(let ((,var ,(expand-convert-value value-var type)))
       ,@body)))

(defgeneric expand-callback-dynamic-extent (var raw-value body type)
  (:method (var raw-value body type)
    `(let ((,var ,(expand-translate-value raw-value type)))
       (declare (type ,(lisp-type type) ,var))
       ,@body)))

(defgeneric allocate-value (value type)
  (:method (value type)
    (foreign-alloc :uint8 :count (compute-size value type)))
  (:method (value (type primitive-type))
    (foreign-alloc (primitive-type-cffi-type type))))

(defgeneric expand-allocate-value (value-form type)
  (:method (value-form type)
    `(allocate-value ,value-form
                     (parse-typespec ',(unparse-type type))))
  (:method (value-form (type primitive-type))
    `(foreign-alloc ',(primitive-type-cffi-type type))))

(defgeneric clean-value (pointer value type)
  (:method (pointer value type)
    nil))

(defgeneric expand-clean-value (pointer value type)
  (:method (pointer value (type primitive-type))
    nil)
  (:method (pointer value type)
   `(clean-value ,pointer ,value ,type)))

(defgeneric free-value (pointer type)
  (:method (pointer type)
    (foreign-free pointer)
    nil))

(defgeneric expand-free-value (pointer-form type)
  (:method (pointer-form type)
    `(free-value ,pointer-form (parse-typespec ',(unparse-type type))))
  (:method (pointer-form (type primitive-type))
    `(foreign-free ,pointer-form)
    nil))

(defmacro %unwind-protect (form &rest cleanup-forms)
  (if (find (complement #'constantp) cleanup-forms)
    `(unwind-protect
         ,form
       ,@cleanup-forms)
    form))

(defgeneric expand-reference-dynamic-extent
    (var size-var value-var body mode type)
  (:method (var size-var value-var body mode type)
    (with-gensyms (pointer-var)
      `(with-foreign-pointer (,pointer-var ,(eval-if-constantp
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
               ,(expand-clean-value pointer-var value-var type))))))))

(defun eval-if-constantp (x)
  (if (constantp x)
    (values (eval x) T)
    (values x nil)))

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
          `(let* ((,value ,(if value-p
                             value-form
                             (expand-prototype type)))
                  (,pointer ,(expand-allocate-value value type)))
             (declare (type pointer ,pointer))
             ,(expand-write-value value pointer type)
             ,pointer)))
      form)))

(defun free (pointer type)
  (declare (type pointer pointer))
  (free-value pointer (parse-typespec type)))

(define-compiler-macro free (&whole form pointer type)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      (expand-free-value pointer (parse-typespec type))
      form)))

(defun clean (pointer value type)
  (declare (type pointer pointer))
  (clean-value pointer value (parse-typespec type)))

(define-compiler-macro clean (&whole form pointer value type)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      (expand-clean-value pointer value (parse-typespec type))
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

(defun convert (lisp-value type)
  (convert-value lisp-value (parse-typespec type)))

(define-compiler-macro convert (lisp-value type)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      (expand-convert-value lisp-value (parse-typespec type))
      `(convert-value ,lisp-value (parse-typespec ,type)))))

(defun translate (raw-value type)
  (translate-value raw-value (parse-typespec type)))

(define-compiler-macro translate (raw-value type)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      (expand-translate-value raw-value (parse-typespec type))
      `(translate-value ,raw-value (parse-typespec ,type)))))

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
