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

(define-immediate-type reference-type ()
  ((referenced-type :initarg :type :initform nil
                    :reader rtype-type)
   (mode :initarg :mode :initform :in :reader rtype-mode)
   (voidable-p :initarg :voidable :initform nil
               :reader rtype-voidable-p))
  (:base-type pointer)
  (:lisp-type (type)
    (let ((rtype (lisp-type (rtype-type type))))
      (if (rtype-voidable-p type)
        `(or void ,rtype)
        rtype)))
  (:prototype (type)
    (prototype (rtype-type type)))
  (:prototype-expansion (type)
    (expand-prototype (rtype-type type)))
  (:converter (lisp-value type)
    (let ((rtype (rtype-type type)))
      (if (rtype-voidable-p type)
        (if (voidp lisp-value)
          &0
          (let ((pointer (allocate-value lisp-value rtype)))
            (declare (type pointer pointer))
            (write-value lisp-value pointer rtype)
            pointer))
        (let ((pointer (allocate-value lisp-value rtype)))
          (declare (type pointer pointer))
          (write-value lisp-value pointer rtype)
          pointer))))
  (:translator (raw-value type)
    (if (rtype-voidable-p type)
      (if (&? raw-value)      
        (read-value raw-value nil (rtype-type type))
        void)
      (read-value raw-value nil (rtype-type type))))
  (:cleaner (pointer value type)
    (let ((ref (deref pointer '*))
          (rtype (rtype-type type)))
      (if (rtype-voidable-p type)
        (when (&? ref)
          (clean-value ref value rtype)
          (free-value ref rtype))
        (progn
          (clean-value ref value rtype)
          (free-value ref rtype)))))
  (:converter-expansion (lisp-value type)
    (let ((rtype (rtype-type type)))
      (once-only (lisp-value)
        (with-gensyms (pointer)
          (let ((expansion
                  `(let ((,pointer ,(expand-allocate-value lisp-value rtype)))
                     (declare (type pointer ,pointer))
                     ,(expand-write-value lisp-value pointer rtype)
                     ,pointer)))
            (if (rtype-voidable-p type)
              `(if (voidp ,lisp-value)
                 &0
                 ,expansion)
              expansion))))))
  (:translator-expansion (raw-value-form type)
    (with-gensyms (raw-value)
      `(let ((,raw-value ,raw-value-form))
         (declare (type pointer ,raw-value))
         ,(if (rtype-voidable-p type)
            `(if (&? ,raw-value)         
               ,(expand-read-value raw-value nil (rtype-type type))
               void)
            (expand-read-value raw-value nil (rtype-type type))))))
  (:allocator-expansion (value type)
    `(foreign-alloc :uint8 :count ,(compute-fixed-size type)))
  (:cleaner-expansion (pointer value type)
    (with-gensyms (ref)
      (once-only (value)
        `(let ((,ref (deref ,pointer '*)))
           (declare (type pointer ,ref)
                    (type ,(lisp-type (rtype-type type)) ,value))
           ,(if (rtype-voidable-p type)
              `(when (&? ,ref)
                 ,(expand-clean-value ref value (rtype-type type))
                 ,(expand-free-value ref (rtype-type type)))
              `(progn
                 ,(expand-clean-value ref value (rtype-type type))
                ,(expand-free-value ref (rtype-type type))))))))
  (:deallocator-expansion (pointer type)
    `(foreign-free ,pointer))
  (:dynamic-extent-expansion (var value-var body type)
    (let ((expansion
            (expand-reference-dynamic-extent
              var (gensym) value-var body
              (rtype-mode type) (rtype-type type))))
      (if (rtype-voidable-p type)
        `(if (voidp ,value-var)
           (let ((,var &0))
             (prog1
              (progn ,@body)
              ,(when (member (rtype-mode type) '(:out :inout))
                 `(setf ,value-var void))))
           ,expansion)
        expansion)))
  (:callback-dynamic-extent-expansion (var raw-value body type)
    (let ((mode (rtype-mode type))
          (rtype (rtype-type type))
          (voidable (rtype-voidable-p type)))
      (if (eq mode :in)
        (call-next-method)
        (with-gensyms (pointer)
          `(let ((,pointer ,raw-value))
             (declare (type pointer ,pointer))
             (let ((,var ,(if (eq mode :out)
                            (expand-prototype type)
                            (expand-translate-value pointer type))))
               (declare (type ,(lisp-type type) ,var))
               (prog1 (progn ,@body)
                ,(if voidable
                   `(when (and (&? ,pointer)
                               (not (voidp ,var)))
                      ,(expand-write-value var pointer rtype))
                   (expand-write-value var pointer rtype))))))))))

(defmethod expand-reference-dynamic-extent
    (var size-var value-var body mode (type reference-type))
  (with-gensyms (pointer-var)
    `(with-foreign-pointer (,var ,(compute-fixed-size type) ,size-var)
       ,(expand-reference-dynamic-extent
          pointer-var (gensym) value-var
          `((setf (deref ,var '*) ,pointer-var) nil ,@body)
          mode (rtype-type type)))))

(define-type-parser & (referenced-type &optional (mode :in) voidable)
  (check-type mode (member :in :out :inout))
  (make-instance 'reference-type
    :type (parse-typespec referenced-type)
    :mode mode
    :voidable voidable))

(defmethod unparse-type ((type reference-type))
  (list* '& (unparse-type (rtype-type type)) (rtype-mode type)
         (ensure-list (rtype-voidable-p type))))

(defun ensure-var-spec (spec)
  (destructuring-bind
      (var &optional (size-var (gensym)) &rest rest)
      (ensure-list spec)
    (if (and (not (constantp var))
             (not (constantp size-var))
             (symbolp var)
             (symbolp size-var)
             (not (eq var size-var))
             (null rest))
      (values var size-var)
      (error "Ill-formed variable spec: ~s" spec))))

(defmacro with-reference ((var value-var type &optional (mode :in) voidable)
                          &body body)
  (check-type mode (member :in :out :inout))
  (check-type value-var symbol)
  (assert (not (constantp value-var)) (value-var))
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (multiple-value-bind
        (var size-var) (ensure-var-spec var)
      (let ((expansion
              (if constantp
                (expand-reference-dynamic-extent
                  var size-var value-var body mode (parse-typespec type))
                (with-gensyms (type-var pointer-var)
                  `(let* ((,type-var (parse-typespec ,type))
                          (,size-var (compute-size ,value-var ,type-var))
                          (,pointer-var (allocate-value ,value-var ,type-var))
                          (,var ,pointer-var))
                     (declare (type pointer ,var ,pointer-var)
                              (type non-negative-fixnum ,size-var)
                              (ignorable ,size-var))
                     (unwind-protect
                         ,(case mode
                            (:in `(progn
                                    (write-value ,value-var ,pointer-var ,type-var)
                                    ,@body))
                            (:out `(prog1
                                    (progn ,@body)
                                    (setf ,value-var
                                          (read-value
                                            ,pointer-var ,value-var ,type-var))))
                            (:inout `(progn (write-value
                                              ,value-var ,pointer-var ,type-var)
                                            (prog1
                                             (progn ,@body)
                                             (setf ,value-var
                                                   (read-value
                                                     ,pointer-var ,value-var ,type-var))))))
                       (progn                             
                         (clean-value ,pointer-var ,value-var ,type-var)
                         (free-value ,pointer-var ,type-var))))))))
        (if voidable
          `(if (voidp ,value-var)
             (let ((,var &0)
                   (,size-var 0))
               (declare (ignorable ,size-var))
               (prog1 (progn ,@body)
                ,(when (member mode '(:out :inout))
                   `(setf ,value-var void))))
             ,expansion)
          expansion)))))

(defmacro with-references ((&rest specs) &body body)
  (if (endp specs)
    `(progn ,@body)
    `(with-reference ,(first specs)
       (with-references ,(rest specs)
         ,@body))))

(defmacro with-pointer ((var value type &optional (mode :in) voidable) &body body)
  (with-gensyms (value-var)
    `(let ((,value-var ,value))
       (declare (ignorable ,value-var))
       (with-reference (,var ,value-var ,type ,mode ,voidable)
         ,@body))))

(defmacro with-pointers ((&rest specs) &body body)
  (if (endp specs)
    `(progn ,@body)
    `(with-pointer ,(first specs)
       (with-pointers ,(rest specs)
         ,@body))))

(defmacro with-value ((var pointer-form type &optional (mode :in) voidable)
                      &body body)
  (check-type mode (member :in :out :inout))
  (multiple-value-bind
      (type constantp) (eval-if-constantp type)
    (if constantp
      (expand-callback-dynamic-extent
        var
        pointer-form
        body
        (make-instance 'reference-type
          :type (parse-typespec type)
          :mode mode
          :voidable (not (null voidable))))
      (with-gensyms (pointer type-var)
        `(let ((,pointer ,pointer-form)
               (,type-var (parse-typespec ',type)))
           (declare (type pointer ,pointer))
           ,(let ((expansion
                    (ecase mode
                      (:in `(let ((,var (read-value ,pointer nil ,type-var)))
                              ,@body))
                      (:out `(let ((,var (prototype ,type-var)))
                               (prog1 (progn ,@body)
                                (write-value ,var ,pointer ,type-var))))
                      (:inout `(let ((,var (read-value ,pointer nil ,type-var)))
                                 (prog1 (progn ,@body)
                                  (write-value ,var ,pointer ,type-var)))))))
              (if voidable
                `(if (&? ,pointer)
                   ,expansion
                   (let ((,var void))
                     ,@body))
                expansion)))))))

(defmacro with-values ((&rest specs) &body body)
  (if (endp specs)
    `(progn ,@body)
    `(with-value ,(first specs)
       (with-values ,(rest specs)
         ,@body))))
