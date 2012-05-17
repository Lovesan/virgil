;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2010-2012, Dmitry Ignatiev <lovesan.ru@gmail.com>

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

(defvar *eq-constants* (make-hash-table :test #'eq))
(defvar *eql-constants* (make-hash-table :test #'eql))
(defvar *equal-constants* (make-hash-table :test #'equal))
(defvar *equalp-constants* (make-hash-table :test #'equalp))

(define-proxy-type const-type ()
  ((mode :initarg :mode
         :initform 'equal
         :reader const-type-mode))
  (:allocator (value type)
    (let* ((ctype (proxied-type type))
           (mode (const-type-mode type))
           (pointer (ecase mode
                      (eq (gethash value *eq-constants*))
                      (eql (gethash value *eql-constants*))
                      (equal (gethash value *equal-constants*))
                      (equalp (gethash value *equalp-constants*)))))
      (unless pointer
        (setf pointer (allocate-value value ctype))
        (write-value value pointer ctype)
        (setf (gethash value (ecase mode
                               (eq *eq-constants*)
                               (eql *eql-constants*)
                               (equal *equal-constants*)
                               (equalp *equalp-constants*)))
              pointer))
      pointer))
  (:allocator-expansion (value type)
    (let* ((ctype (proxied-type type))
           (mode (const-type-mode type))
           (hash (ecase mode
                   (eq '*eq-constants*)
                   (eql '*eql-constants*)
                   (equal '*equal-constants*)
                   (equalp '*equalp-constants*))))
      (once-only (value)
        (with-gensyms (pointer)
          `(or (gethash ,value ,hash)
               (setf (gethash ,value ,hash)
                     (let ((,pointer ,(expand-allocate-value
                                        value
                                        ctype)))
                       (declare (type pointer ,pointer))
                       ,(expand-write-value value pointer ctype)
                       ,pointer)))))))
  (:deallocator (pointer type) nil)
  (:deallocator-expansion (pointer type) nil)
  (:cleaner (value pointer type) nil)
  (:cleaner-expansion (value pointer type) nil)
  (:reference-dynamic-extent-expansion
    (var size-var value-var body mode type)
    `(let ((,var ,(expand-allocate-value value-var type))
           (,size-var ,(expand-compute-size value-var type)))
       (declare (type pointer ,var)
                (type size-t ,size-var)
                (ignorable ,size-var))
       ,(ecase mode
          (:in `(locally ,@body))
          ((:inout :out) (error "Trying to modify value of const type, do you?"))))))

(define-immediate-type const-immediate-type (const-type)
  ())

(define-type-parser const (type &optional (comparator 'equal))
  (check-type comparator (member eq eql equal equalp))
  (let ((type (parse-typespec type)))
    (make-instance (if (immediate-type-p type)
                     'const-immediate-type
                     'const-type)
      :mode comparator
      :type type)))

(defmethod unparse-type ((type const-type))
  `(const ,(unparse-type (proxied-type type))
          ,(const-type-mode type)))

(defun clear-const-cache ()
  (loop :for hash :in (list *eq-constants*
                            *eql-constants*
                            *equal-constants*
                            *equalp-constants*)
    :do (maphash (lambda (k v) (declare (ignore k)) (raw-free v)) hash)
    (clrhash hash)))
