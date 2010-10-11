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

(define-primitive-type pointer
  (:cffi-type :pointer)
  (:lisp-type foreign-pointer)
  (:prototype (null-pointer)))

(defalias * () 'pointer)

(declaim (inline &p))
(defun &p (object)
  (pointerp object))

(declaim (inline &))
(defun & (address)
  (declare (type size-t address))
  (make-pointer address))

(declaim (inline &&))
(defun && (pointer)
  (declare (type pointer pointer))
  (pointer-address pointer))

(declaim (inline &=))
(defun &= (pointer1 pointer2)
  (declare (type pointer pointer1 pointer2))
  (pointer-eq pointer1 pointer2))

(declaim (inline &+))
(defun &+ (pointer offset &optional (type 'uint8))
  (declare (type pointer pointer)
           (type fixnum offset))
  (inc-pointer pointer (* offset (compute-fixed-size (parse-typespec type)))))

(define-compiler-macro &+ (pointer offset &optional (type ''uint8))
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      `(inc-pointer ,pointer (* ,offset ,(compute-fixed-size
                                           (parse-typespec type))))
      `(inc-pointer ,pointer (* ,offset (compute-fixed-size
                                          (parse-typespec ,type)))))))

(declaim (inline &-))
(defun &- (pointer offset &optional (type 'uint8))
  (declare (type pointer pointer)
           (type fixnum offset))
  (inc-pointer pointer (- (* offset (compute-fixed-size (parse-typespec type))))))

(define-compiler-macro &- (pointer offset &optional (type ''uint8))
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      `(inc-pointer ,pointer (- (* ,offset ,(compute-fixed-size
                                              (parse-typespec type)))))
      `(inc-pointer ,pointer (- (* ,offset (compute-fixed-size
                                             (parse-typespec ,type))))))))

(declaim (inline &?))
(defun &? (pointer)
  (declare (type pointer pointer))
  (not (null-pointer-p pointer)))

(declaim (inline &0))
(defun &0 ()
  (null-pointer))

(define-symbol-macro &0 (&0))

(defun deref (pointer type &optional (offset 0) output)
  (declare (type pointer pointer)
           (type non-negative-fixnum offset))
  (read-value (&+ pointer offset) output (parse-typespec type)))

(define-compiler-macro deref (pointer type &optional (offset 0) output)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      (expand-read-value `(&+ ,pointer ,offset) output
                         (parse-typespec type))
      `(read-value (&+ ,pointer ,offset) ,output
                   (parse-typespec ,type)))))

(defun (setf deref) (value pointer type &optional (offset 0))
  (declare (type pointer pointer)
           (type non-negative-fixnum offset))
  (write-value value (&+ pointer offset) (parse-typespec type)))

(define-compiler-macro (setf deref) (value pointer type &optional (offset 0))
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      (expand-write-value value `(&+ ,pointer ,offset)
                          (parse-typespec type))
      `(write-value ,value (&+ ,pointer ,offset)
                    (parse-typespec ,type)))))
