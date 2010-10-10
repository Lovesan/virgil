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

(define-primitive-type char-t
  (:cffi-type :char)
  (:lisp-type #.(%int-type :char)))

(define-primitive-type uchar-t
  (:cffi-type :uchar)
  (:lisp-type #.(%int-type :uchar)))

(define-primitive-type wchar-t
  (:cffi-type #+windows :uint16
              #-windows :uint32)
  (:lisp-type #.(%int-type #+windows :uint16
                           #-windows :uint32)))

(define-primitive-type short
  (:cffi-type :short)
  (:lisp-type #.(%int-type :short)))

(define-primitive-type ushort
  (:cffi-type :ushort)
  (:lisp-type #.(%int-type :ushort)))

(define-primitive-type int
  (:cffi-type :int)
  (:lisp-type #.(%int-type :int)))

(define-primitive-type uint
  (:cffi-type :uint)
  (:lisp-type #.(%int-type :uint)))

(define-primitive-type long
  (:cffi-type :long)
  (:lisp-type #.(%int-type :long)))

(define-primitive-type ulong
  (:cffi-type :ulong)
  (:lisp-type #.(%int-type :ulong)))

(define-primitive-type llong
  (:cffi-type :llong)
  (:lisp-type #.(%int-type :llong)))

(define-primitive-type ullong
  (:cffi-type :ullong)
  (:lisp-type #.(%int-type :ullong)))

(define-primitive-type uint-ptr
  (:cffi-type #-x86-x64 :uint32
              #+x86-x64 :uint64)
  (:lisp-type #-x86-x64 #.(%int-type :uint32)
              #+x86-64 #.(%int-type :uint64)))

(define-primitive-type int-ptr
  (:cffi-type #-x86-x64 :int32
              #+x86-x64 :int64)
  (:lisp-type #-x86-x64 #.(%int-type :int32)
              #+x86-x64 #.(%int-type :int64)))

(define-primitive-type single
  (:cffi-type :float)
  (:lisp-type single-float))

(define-primitive-type double
  (:cffi-type :double)
  (:lisp-type double-float))

(define-primitive-type bool
  (:cffi-type :boolean)
  (:lisp-type boolean)
  (:prototype nil))

(define-primitive-type int8
  (:cffi-type :int8)
  (:lisp-type #.(%int-type :int8)))
(define-primitive-type uint8
  (:cffi-type :uint8)
  (:lisp-type #.(%int-type :uint8)))
(define-primitive-type int16
  (:cffi-type :int16)
  (:lisp-type #.(%int-type :int16)))
(define-primitive-type uint16
  (:cffi-type :uint16)
  (:lisp-type #.(%int-type :uint16)))
(define-primitive-type int32
  (:cffi-type :int32)
  (:lisp-type #.(%int-type :int32)))
(define-primitive-type uint32
  (:cffi-type :uint32)
  (:lisp-type #.(%int-type :uint32)))
(define-primitive-type int64
  (:cffi-type :int64)
  (:lisp-type #.(%int-type :int64)))
(define-primitive-type uint64
  (:cffi-type :uint64)
  (:lisp-type #.(%int-type :uint64)))

(defalias size-t () 'uint-ptr)
(deftype size-t () 'uint-ptr)
(defalias ssize-t () 'int-ptr)
(deftype ssize-t () 'int-ptr)

(defalias ptrdiff-t () 'ssize-t)
(deftype ptrdiff-t () 'ssize-t)

(defalias sbyte () 'int8)
(deftype sbyte () 'int8)
(defalias ubyte () 'uint8)
(deftype ubyte () 'uint8)
(defalias byte () 'uint8)

(defalias float () 'single)

(define-immediate-type generic-char-type ()
  ()  
  (:prototype (type) (code-char 0))
  (:prototype-expansion (type) #.(code-char 0))
  (:translator (raw-value type)
    (code-char raw-value))
  (:converter (lisp-value type)
    (char-code lisp-value))
  (:translator-expansion (raw-value-form type)
    `(code-char ,raw-value-form))
  (:converter-expansion (lisp-value-form type)
    `(char-code ,lisp-value-form))
  (:cleaner (pointer value type) nil)
  (:cleaner-expansion (pointer value type) nil))

(define-immediate-type char-type (generic-char-type)
  ()
  (:base-type uchar-t)
  (:simple-parser char)
  (:lisp-type (type) 'base-char))

(define-immediate-type wchar-type (generic-char-type)
  ()
  (:base-type wchar-t)
  (:simple-parser wchar)
  (:lisp-type (type) 'character))

(define-immediate-type boolean-type ()
  ()
  (:base-type int)
  (:simple-parser boolean)
  (:lisp-type (type) t)
  (:prototype (type) nil)
  (:prototype-expansion (type) nil)
  (:converter (lisp-value type)
    (if lisp-value 1 0))
  (:converter-expansion (lisp-value-form type)
    `(if ,lisp-value-form 1 0))
  (:translator (raw-value type)
    (/= 0 raw-value))
  (:translator-expansion (raw-value-form type)
    `(/= 0 ,raw-value-form))
  (:cleaner (pointer value type) nil)
  (:cleaner-expansion (pointer value type) nil))

(defun error-void-operation ()
  (error "Cannot operate on VOID type"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline %void voidp))
  (defstruct (void (:constructor %void ())
                   (:predicate voidp)
                   (:copier)
                   (:print-object
                     (lambda (o s)                       
                       (print-unreadable-object (o s)
                         (write 'void :stream s)))))))

(define-constant void (%void) :test #'equalp)

(define-immediate-type void-type ()
  ((base-type :initform (make-primitive-type
                          :name :void
                          :cffi-type :void
                          :lisp-type 'void
                          :prototype void)))
  (:simple-parser void)
  (:lisp-type (type) T)
  (:prototype (type) void)
  (:prototype-expansion (type) 'void)
  (:converter (val type) void)
  (:converter-expansion (val type) 'void)
  (:translator (val type) void)
  (:translator-expansion (val type) 'void)
  (:cleaner (p v type) (error-void-operation))
  (:cleaner-expansion (p v type) (error-void-operation))
  (:dynamic-extent-expansion (var val body type)
    (error-void-operation)))

(defmethod compute-fixed-size ((type void-type))
  (error-void-operation))
(defmethod compute-size (val (type void-type))
  (error-void-operation))
(defmethod expand-compute-size (var (type void-type))
  (error-void-operation))
(defmethod compute-alignment ((type void-type))
  (error-void-operation))
(defmethod read-value (ptr out (type void-type))
  (error-void-operation))
(defmethod write-value (ptr out (type void-type))
  (error-void-operation))
(defmethod expand-read-value (ptr out (type void-type))
  (error-void-operation))
(defmethod expand-write-value (ptr out (type void-type))
  (error-void-operation))
(defmethod expand-reference-dynamic-extent
    (var szvar valvar body mode (type void-type))
  (error-void-operation))
(defmethod expand-callback-dynamic-extent
    (var value body (type void-type))
  (error-void-operation))
