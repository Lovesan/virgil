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

(defvar *enum-type-hash* (make-hash-table :test #'eq))

(define-immediate-type enum-type ()
  ((keys->values :initarg :kv
                 :initform '()
                 :reader enum-type-kv)
   (values->keys :initarg :vk
                 :initform '()
                 :reader enum-type-vk))
  (:base-type int)
  (:prototype (type) 0)
  (:prototype-expansion (type) 0)
  (:lisp-type (type)
    `(or (member ,@(mapcar #'first (enum-type-kv type)))
         ,(lisp-type (base-type type))))
  (:converter (lisp-value type)
    (etypecase lisp-value
      (integer lisp-value)
      (keyword (or (second (assoc lisp-value (enum-type-kv type)))
                   (error "~s is invalid keyword for enum type ~s"
                          lisp-value (unparse-type type))))))
  (:translator (raw-value type)
    (or (second (assoc raw-value (enum-type-vk type) :test #'=))
        raw-value))
  (:converter-expansion (lisp-value-form type)
    (with-gensyms (lisp-value)
      `(let ((,lisp-value ,lisp-value-form))
         (declare (type ,(lisp-type type) ,lisp-value))
         (etypecase ,lisp-value
           (integer ,lisp-value)
           (keyword (case ,lisp-value
                      ,@(enum-type-kv type)
                      (T (error "~s is invalid keyword for enum type ~s"
                                ,lisp-value
                                ',(unparse-type type)))))))))
  (:translator-expansion (raw-value-form type)
    (with-gensyms (raw-value)
      `(let ((,raw-value ,raw-value-form))
         (declare (type ,(lisp-type (base-type type))
                        ,raw-value))
         (case ,raw-value
           ,@(enum-type-vk type)
           (T ,raw-value)))))
  (:cleaner (pointer value type) nil)
  (:cleaner-expansion (pointer value type) nil)
  (:allocator-expansion (value type)
    `(foreign-alloc :uint8 :count ,(compute-fixed-size type)))
  (:deallocator-expansion (pointer type)
    `(foreign-free ,pointer)))

(define-aggregate-type named-enum-type (enum-type)
  ((name :initarg :name
         :initform nil
         :reader enum-type-name))
  (:lisp-type (type)
    (enum-type-name type)))

(defun parse-enum-list (enum-list)
  (loop :with kv = '()
    :with vk = '()
    :for x :in enum-list
    :for i :from 0
    :do (cond
          ((keywordp x)
           (push (list x i) kv)
           (push (list i x) vk))
          ((consp x)
           (destructuring-bind
               (k v &rest rest) x
             (if (and (integerp v)
                      (keywordp k)
                      (null rest))
               (progn (push (list k v) kv)
                      (push (list v k) vk)
                      (setf i v))
               (error "Invalid enum value spec: ~s" x))))
          (T (error "Invalid enum value spec: ~s" x)))
    :finally (return (values kv vk))))

(define-type-parser enum (options &rest enum-list)
  (destructuring-bind
    (&key (base-type 'int)) options
    (multiple-value-bind
        (kv vk) (parse-enum-list enum-list)
      (make-instance 'enum-type
        :kv kv :vk vk
        :base-type (parse-typespec base-type)))))

(defmethod unparse-type ((type enum-type))
  `(enum (:base-type ,(unparse-type (base-type type)))
         ,@(enum-type-kv type)))

(defmethod unparse-type ((type named-enum-type))
  (enum-type-name type))

(defmacro define-enum (name-and-options &rest enum-list)
  (check-type name-and-options (or symbol cons))
  (let* ((name-and-options (ensure-list name-and-options))
         (name (first name-and-options))
         (options (rest name-and-options)))
    (assert (and (symbolp name)
                 (not (constantp name))
                 (listp options))
        (name options))
    (destructuring-bind
        (&key (conc-name (intern (format nil "~a-" name)))
              (base-type 'int))
        (flatten-options options)
      (multiple-value-bind
          (kv vk) (parse-enum-list enum-list)
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (gethash ',name *enum-type-hash*)
                 (make-instance 'named-enum-type
                   :base-type (parse-typespec ',base-type)
                   :name ',name
                   :kv ',kv
                   :vk ',vk))
           (define-type-parser ,name ()
             (gethash ',name *enum-type-hash*))
           ,@(loop :for (k v) :in kv
               :collect `(defconstant ,(intern (format nil "~a~a" conc-name k))
                           ,v))
           (deftype ,name () 
             '(or (member ,@(mapcar #'first kv))
               ,(lisp-type (parse-typespec base-type)))))))))
