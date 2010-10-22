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

(in-package #:virgil-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *tests* '()))

(defmacro deftest (name &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *tests*)))

(defun run-tests ()
  (loop :for name :in (reverse *tests*)
    :do (format t "~&~a:~%~8T" name)
    (handler-case
        (format t
                (if (funcall name)
                  "PASSED"
                  "FAILED"))
      (error (e) (format t "UNEXPECTED FAILURE:~%~a" e)))
    (terpri)
    (force-output *error-output*)))

(defvar *primitives* (list 'char-t 123
                           'uchar-t  123
                           'wchar-t  123
                           'byte  123
                           'ubyte 123
                           'short 123
                           'ushort 123
                           'int 123
                           'uint 123
                           'long 123
                           'ulong  123
                           'llong  123
                           'ullong 123
                           'pointer (cffi:null-pointer)
                           'single 123.0s0
                           'double 123.0d0
                           'char #\A
                           'wchar #\A
                           ))

(defun compiled (form)  
  (funcall
    (compile nil `(lambda ()
                    #+sbcl (declare (sb-ext:muffle-conditions
                                     sb-ext:compiler-note))
                    ,form))))

(deftest primitives.convert-translate
  (loop :for (type test) :on *primitives* :by #'cddr
    :always (and (eql test (translate (convert test type) type))
                 (compiled
                   `(eql ,test (translate (convert ,test ',type) ',type))))))

(defun eqlp (x y)
  (if (and (&p x) (&p y))
    (&= x y)
    (eql x y)))
  
(deftest primitives.alloc-read-write-free
    (loop :for (type test) :on *primitives* :by #'cddr
      :always
      (and (let ((p (alloc type test)))
             (unwind-protect
                 (eqlp test (deref p type))
               (free p type)))
           (compiled
             `(let ((p (alloc ',type ,test)))
                (unwind-protect
                    (eqlp ,test (deref p ',type))
                  (free p ',type))))
           (compiled
             `(with-pointer (p ,test ',type)
                (eqlp ,test (deref p ',type)))))))

(deftest enums.convert-translate
    (let ((ts '(enum (:base-type uint32)
                :c1
                :c2
                (:c3 123)
                :c4)))
      (and (eq :c1 (translate (convert 0 ts) ts))
           (eq :c2 (translate (convert 1 ts) ts))
           (eq :c3 (translate (convert 123 ts) ts))
           (eq :c4 (translate (convert 124 ts) ts))
           (compiled
             `(and (eq :c1 (translate (convert 0 ',ts) ',ts))
                   (eq :c2 (translate (convert 1 ',ts) ',ts))
                   (eq :c3 (translate (convert 123 ',ts) ',ts))
                   (eq :c4 (translate (convert 124 ',ts) ',ts)))))))

(define-enum (test-enum (:base-type uint32))
  :c1
  :c2
  (:c3 123)
  :c4)

(deftest enums.named.convert-translate
    (let ((ts 'test-enum))
      (and (eq :c1 (translate (convert 0 ts) ts))
           (eq :c2 (translate (convert 1 ts) ts))
           (eq :c3 (translate (convert 123 ts) ts))
           (eq :c4 (translate (convert 124 ts) ts))
           (compiled
             `(and (eq :c1 (translate (convert 0 ',ts) ',ts))
                   (eq :c2 (translate (convert 1 ',ts) ',ts))
                   (eq :c3 (translate (convert 123 ',ts) ',ts))
                   (eq :c4 (translate (convert 124 ',ts) ',ts)))))))

(deftest enums.alloc-read-write-free
    (let* ((ts '(enum (:base-type uint32) (:c 123))))
      (and (let ((p (alloc ts :c)))
             (unwind-protect
                 (eq :c (deref p ts))
               (free p ts)))
           (with-pointer (p :c ts)
             (eq :c (deref p ts)))
           (compiled
             `(let ((p (alloc ',ts :c)))
                (unwind-protect
                    (eq :c (deref p ',ts))
                  (free p ',ts))))
           (compiled
             `(with-pointer (p :c ',ts)
                (eq :c (deref p ',ts)))))))

(deftest enums.named.alloc-read-write-free
    (let* ((ts 'test-enum))
      (and (let ((p (alloc ts :c3)))
             (unwind-protect
                 (eq :c3 (deref p ts))
               (free p ts)))
           (with-pointer (p :c3 ts)
             (eq :c3 (deref p ts)))
           (compiled
             `(let ((p (alloc ',ts :c3)))
                (unwind-protect
                    (eq :c3 (deref p ',ts))
                  (free p ',ts))))
           (compiled
             `(with-pointer (p :c3 ',ts)
                (eq :c3 (deref p ',ts)))))))

(defun ht (&rest args)
  (let ((ht (make-hash-table :test #'eq)))
    (loop :for (k v) :on args :by #'cddr
      :do (setf (gethash k ht) v))
    ht))

(defun h (ht k)
  (gethash k ht))

(deftest structs.alloc-read-write-free
    (let ((ts '(struct ()
                (x char)
                (y float))))
      (and (let* ((struct (ht 'x #\A 'y 123.0s0))
                  (out (ht 'x #\B 'y 124.0s0))
                  (p (alloc ts struct)))
             (unwind-protect
                 (and (equalp struct (deref p ts))
                      (eq out (deref p ts 0 out))
                      (equalp out struct))
               (free p ts)))
           (compiled
             `(let* ((struct (ht 'x #\A 'y 123.0s0))
                     (out (ht 'x #\B 'y 124.0s0))
                     (p (alloc ',ts struct)))
                (unwind-protect
                    (and (equalp struct (deref p ',ts))
                         (eq out (deref p ',ts 0 out))
                         (equalp out struct))
                  (free p ',ts)))))))

(define-struct test-struct
  (x char :initform #\A)
  (y float :initform 123.0s0))

(deftest structs.named.alloc-read-write-free
    (let ((ts 'test-struct))
      (and (let* ((struct (make-test-struct))
                  (out (make-test-struct :x #\B :y 124.0s0))
                  (p (alloc ts struct)))
             (unwind-protect
                 (and (equalp struct (deref p ts))
                      (eq out (deref p ts 0 out))
                      (equalp out struct))
               (free p ts)))
           (compiled
             `(let* ((struct (make-test-struct))
                     (out (make-test-struct :x #\B :y 124.0s0))
                     (p (alloc ',ts struct)))
                (unwind-protect
                    (and (equalp struct (deref p ',ts))
                         (eq out (deref p ',ts 0 out))
                         (equalp out struct))
                  (free p ',ts)))))))
