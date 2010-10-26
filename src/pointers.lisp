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


(declaim (inline raw-alloc))
(defun raw-alloc (size)
  (declare (type size-t size))
  #+sbcl (sb-alien:alien-funcall
           (sb-alien:extern-alien
             "malloc"
             (function sb-sys:system-area-pointer sb-alien:unsigned))
           size)
  #-sbcl
  (the pointer (foreign-alloc :uint8 :count size)))

(declaim (inline raw-free))
(defun raw-free (pointer)
  (declare (type pointer pointer))
  #+sbcl (sb-alien:alien-funcall
           (sb-alien:extern-alien
             "free"
             (function sb-alien:void sb-alien:system-area-pointer))
           pointer)
  #-sbcl (foreign-free pointer)
  nil)

(defmacro with-raw-pointer (&whole form
                            (var size &optional (size-var (gensym)))
                            &body body)
  (if (constantp size)
    `(with-foreign-pointer (,var ,(eval size) ,size-var)
       ,@body)
    (if (eq var size-var)
      (error "Variable name and size variable must differ: ~s"
             form)
      (with-gensyms (pointer)
        `(let* ((,size-var ,size)
                (,pointer (raw-alloc ,size-var))
                (,var ,pointer))
           (declare (type pointer ,pointer ,var)                    
                    (type size-t ,size-var)
                    (ignorable ,size-var))
           (unwind-protect
               (progn ,@body)
             (raw-free ,pointer)))))))

(defun deref (pointer type &optional (offset 0) output)
  (declare (type pointer pointer)
           (type non-negative-fixnum offset))
  (read-value (&+ pointer offset) output (parse-typespec type)))

(define-compiler-macro deref (pointer type &optional (offset 0) output)
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    `(progn
       ,(if constantp
          (expand-read-value `(&+ ,pointer ,offset) output
                             (parse-typespec type))
          `(read-value (&+ ,pointer ,offset) ,output
                       (parse-typespec ,type))))))

(defun (setf deref) (value pointer type &optional (offset 0))
  (declare (type pointer pointer)
           (type non-negative-fixnum offset))
  (write-value value (&+ pointer offset) (parse-typespec type)))

(define-compiler-macro (setf deref) (value pointer type &optional (offset 0))
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    `(progn
       ,(if constantp
          (expand-write-value value `(&+ ,pointer ,offset)
                              (parse-typespec type))
          `(write-value ,value (&+ ,pointer ,offset)
                        (parse-typespec ,type))))))

(declaim (inline align-offset))
(defun align-offset (offset align)
  (declare (type size-t offset align))
  (multiple-value-bind
      (q r) (floor offset align)
    (declare (ignore q) (type size-t r))
    (if (zerop r)
      offset
      (let* ((padding (the size-t (- align r)))
             (new-offset (the size-t (+ offset padding))))
        new-offset))))

(defmethod write-value :around (value pointer (type translatable-type))
  (if (and *handle-cycles* (not (immediate-type-p type)))
    (let ((cell (assoc value *written-values* :test #'eq)))
      (if cell
        (car cell)
        (progn (push (cons value pointer) *written-values*)
               (call-next-method))))
    (call-next-method)))

(defmethod read-value :around (pointer out (type translatable-type))
  (if (and *handle-cycles* (not (immediate-type-p type)))
    (or (cdr (assoc pointer *readen-values* :test #'&=))
        (let ((out (or out (prototype type))))
          (push (cons pointer out) *readen-values*)
          (call-next-method pointer out type)))
    (call-next-method)))

(defmethod clean-value :around (pointer value (type translatable-type))
  (if (and *handle-cycles* (not (immediate-type-p type)))
    (unless (member pointer *cleaned-values* :test #'&=)
      (push pointer *cleaned-values*)
      (call-next-method))
    (call-next-method)))

(defmethod expand-write-value :around
  (value-form pointer-form (type translatable-type))
  (if (immediate-type-p type)
    (call-next-method)
    (with-gensyms (pointer value cell)
      `(flet ((%write-value (,value ,pointer)
                ,(call-next-method value pointer type)))
         (let ((,pointer ,pointer-form) (,value ,value-form))
           (declare (type pointer ,pointer) (type ,(lisp-type type) ,value))
           (if *handle-cycles*
             (let ((,cell (member ,value *written-values* :test #'eq)))
               (if ,cell
                 (car ,cell)
                 (progn (push (cons ,value ,pointer) *written-values*)
                        (%write-value ,value ,pointer))))
             (%write-value ,value ,pointer)))))))

(defmethod expand-read-value :around
  (pointer-form out-form (type translatable-type))
  (if (immediate-type-p type)
    (call-next-method)
    (with-gensyms (pointer out)
      `(flet ((%read-value (,pointer ,out)
                ,(call-next-method pointer out type)))
         (let ((,pointer ,pointer-form) (,out ,out-form))
           (declare (type pointer ,pointer)
                    (type (or null ,(lisp-type type)) ,out))
           (if *handle-cycles*
             (or (cdr (assoc ,pointer *readen-values* :test #'&=))
                 (let ((,out (or ,out ,(expand-prototype type))))
                   (declare (type ,(lisp-type type) ,out))
                   (push (cons ,pointer ,out) *readen-values*)
                   (%read-value ,pointer ,out)))
             (%read-value ,pointer ,out)))))))

(defmethod expand-clean-value :around
  (pointer-form value-form (type translatable-type))
  (if (immediate-type-p type)
    (call-next-method)
    (with-gensyms (pointer value)
      `(flet ((%clean-value (,pointer ,value)
                (declare (ignorable ,pointer ,value))
                ,(call-next-method pointer value type)))
         (let ((,pointer ,pointer-form)
               (,value ,value-form))
           (declare (type pointer ,pointer)
                    (type ,(lisp-type type) ,value))
           (if *handle-cycles*
             (unless (member ,pointer *cleaned-values* :test #'&=)
               (push ,pointer *cleaned-values*)
               (%clean-value ,pointer ,value))
             (%clean-value ,pointer ,value)))))))

(defmethod expand-reference-dynamic-extent :around
  (var size-var value-var body mode (type translatable-type))
  (if (immediate-type-p type)
    (call-next-method)
    `(progv (when *handle-cycles* '(*readen-values*
                                    *written-values*
                                    *cleaned-values*))
            (when *handle-cycles* '(() () ()))
       ,(call-next-method))))

(defmethod expand-callback-dynamic-extent :around
  (var value body (type translatable-type))
  (if (immediate-type-p type)
    (call-next-method)
    `(progv (when *handle-cycles* '(*readen-values*
                                    *written-values*
                                    *cleaned-values*))
            (when *handle-cycles* '(() () ()))
        ,(call-next-method))))
