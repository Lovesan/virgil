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

(defvar *eq-constants* (make-hash-table :test #'eq))
(defvar *eql-constants* (make-hash-table :test #'eql))
(defvar *equal-constants* (make-hash-table :test #'equal))
(defvar *equalp-constants* (make-hash-table :test #'equalp))

(define-proxy-type const-type ()
  ((mode :initarg :mode
         :initform 'equal
         :reader const-type-mode))
  (:allocator (value type)
    (let ((ctype (proxied-type type))
          (mode (const-type-mode type)))
      (ecase mode
        (eq (or (gethash value *eq-constants*)
                (setf (gethash value *eq-constants*)
                      (allocate-value value ctype))))
        (eql (or (gethash value *eql-constants*)
                 (setf (gethash value *eql-constants*)
                       (allocate-value value ctype))))
        (equal (or (gethash value *equal-constants*)
                   (setf (gethash value *equal-constants*)
                         (allocate-value value ctype))))
        (equalp (or (gethash value *equalp-constants*)
                    (setf (gethash value *equalp-constants*)
                          (allocate-value value ctype)))))))
  (:allocator-expansion (value type)
    (let* ((ctype (proxied-type type))
           (mode (const-type-mode type))
           (hash (ecase mode
                   (eq '*eq-constants*)
                   (eql '*eql-constants*)
                   (equal '*equal-constants*)
                   (equalp '*equalp-constants*))))
      (once-only (value)
        `(or (gethash ,value ,hash)
             (setf (gethash ,value ,hash)
                   ,(expand-allocate-value value ctype))))))
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
          (:in `(progn ,(expand-write-value value-var var type)
                       nil
                       ,@body))
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
