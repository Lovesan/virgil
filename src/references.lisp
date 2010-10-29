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
   (nullable-p :initarg :nullable :initform nil
               :reader rtype-nullable-p))
  (:base-type pointer)
  (:lisp-type (type)
    (let ((rtype (lisp-type (rtype-type type))))
      (if (rtype-nullable-p type)
        `(or void ,rtype)
        rtype)))
  (:prototype (type)
    (if (rtype-nullable-p type)
      void
      (prototype (rtype-type type))))
  (:prototype-expansion (type)
    (if (rtype-nullable-p type)
      'void
      (expand-prototype (rtype-type type))))
  (:allocator-expansion (value type)
    `(raw-alloc ,(compute-fixed-size type)))
  (:deallocator-expansion (pointer type)
    `(raw-free ,pointer))
  (:dynamic-extent-expansion (var value-var body type)
    (let ((expansion
            (expand-reference-dynamic-extent
              var (gensym) value-var body
              (rtype-mode type) (rtype-type type))))
      (if (rtype-nullable-p type)
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
          (nullable (rtype-nullable-p type)))
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
                ,(if nullable
                   `(when (and (&? ,pointer)
                               (not (voidp ,var)))
                      ,(expand-write-value var pointer rtype))
                   (expand-write-value var pointer rtype))))))))))

(defmethod convert-value (value (type reference-type))
  (let ((rtype (rtype-type type))
        (nullable (rtype-nullable-p type)))
    (labels ((alloc-ref (value)
               (let ((pointer (allocate-value value rtype)))
                 (write-value value pointer rtype)
                 pointer))
             (convert-ref (value)
               (if (and *handle-cycles*
                        (not (immediate-type-p rtype)))
                 (or (cdr (assoc value *written-values* :test #'eq))
                     (alloc-ref value))
                 (alloc-ref value))))
      (if nullable
        (if (voidp value)
          &0
          (convert-ref value))
        (convert-ref value)))))

(defmethod expand-convert-value (value (type reference-type))
  (let ((rtype (rtype-type type))
        (nullable (rtype-nullable-p type)))
    (once-only ((value `(the ,(lisp-type type) ,value)))
      `(flet ((alloc-ref (,value)
                ,(with-gensyms (pointer)
                   `(let ((,pointer ,(expand-allocate-value value rtype)))
                      (declare (type pointer ,pointer))
                      ,(expand-write-value value pointer rtype)
                      ,pointer))))
         (flet ((convert-ref (,value)
                  ,(if (immediate-type-p rtype)
                     `(alloc-ref ,value)
                     `(if *handle-cycles*
                        (or (cdr (assoc ,value *written-values* :test #'eq))
                            (alloc-ref ,value))
                        (alloc-ref ,value)))))
           ,(if nullable
              `(if (voidp ,value)
                 &0
                 (convert-ref ,value))
              `(convert-ref ,value)))))))

(defmethod translate-value (pointer (type reference-type))
  (let ((rtype (rtype-type type))
        (nullable (rtype-nullable-p type)))
    (labels ((translate-ref (pointer)
               (read-value pointer nil rtype)))
      (if nullable
        (if (&? pointer)
          (translate-ref pointer)
          void)
        (translate-ref pointer)))))

(defmethod expand-translate-value (pointer (type reference-type))
  (let ((rtype (rtype-type type))
        (nullable (rtype-nullable-p type)))
    (once-only ((pointer `(the pointer ,pointer)))
      `(flet ((read-ref (,pointer)
                ,(expand-read-value pointer nil rtype)))
         ,(if nullable
            `(if (&? ,pointer)
               (read-ref ,pointer)
               void)
            `(read-ref ,pointer))))))

(defmethod clean-value (pointer value (type reference-type))
  (let ((rtype (rtype-type type))
        (nullable (rtype-nullable-p type)))
    (labels ((clean-ref (reference value)
               (if (and *handle-cycles*
                        (not (immediate-type-p rtype)))
                 (unless (member reference *cleaned-values* :test #'&=)
                   (clean-value reference value rtype)
                   (free-value reference rtype))
                 (progn
                   (clean-value reference value rtype)
                   (free-value reference rtype)))))
      (let ((reference (deref pointer 'pointer)))
        (if nullable
          (when (&? reference)
            (clean-ref reference value))
          (clean-ref reference value))))))

(defmethod expand-clean-value (pointer value (type reference-type))
  (let ((rtype (rtype-type type))
        (nullable (rtype-nullable-p type))
        (reference (gensym (string 'reference))))
    (once-only ((pointer `(the pointer ,pointer))
                (value `(the ,(lisp-type type) ,value)))
      `(flet ((clean-ref (,reference ,value)
                ,(expand-clean-value reference value rtype)
                ,(expand-free-value reference rtype)))
         ,(let ((expansion (if (immediate-type-p rtype)
                             `(clean-ref ,reference ,value)
                             `(if *handle-cycles*
                                (unless (member ,reference *cleaned-values* :test #'&=)
                                  (clean-ref ,reference ,value))
                                (clean-ref ,reference ,value)))))
            `(let ((,reference (deref ,pointer 'pointer)))
               (declare (type pointer ,reference))
               ,(if nullable
                  `(when (&? ,reference)
                     ,expansion)
                  expansion)))))))

(defmethod expand-reference-dynamic-extent
    (var size-var value-var body mode (type reference-type))
  (with-gensyms (pointer-var)
    `(with-raw-pointer (,var ,(compute-fixed-size type) ,size-var)
       ,(expand-reference-dynamic-extent
          pointer-var (gensym) value-var
          `((setf (deref ,var '*) ,pointer-var) nil ,@body)
          mode (rtype-type type)))))

(define-type-parser & (referenced-type &optional (mode :in) nullable)
  (check-type mode (member :in :out :inout))
  (make-instance 'reference-type
    :type (parse-typespec referenced-type)
    :mode mode
    :nullable nullable))

(defmethod unparse-type ((type reference-type))
  (list* '& (unparse-type (rtype-type type)) (rtype-mode type)
         (ensure-list (rtype-nullable-p type))))

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

(defmacro with-reference ((var value-var type &optional (mode :in) nullable)
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
                  `(progv (when *handle-cycles*
                            '(*readen-values*
                              *written-values*
                              *cleaned-values*))
                          (when *handle-cycles* '(() () ()))
                     (let* ((,type-var (parse-typespec ,type))
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
                           (free-value ,pointer-var ,type-var)))))))))
        (if nullable
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

(defmacro with-pointer ((var value type &optional (mode :in) nullable) &body body)
  (with-gensyms (value-var)
    `(let ((,value-var ,value))
       (declare (ignorable ,value-var))
       (with-reference (,var ,value-var ,type ,mode ,nullable)
         ,@body))))

(defmacro with-pointers ((&rest specs) &body body)
  (if (endp specs)
    `(progn ,@body)
    `(with-pointer ,(first specs)
       (with-pointers ,(rest specs)
         ,@body))))

(defmacro with-value ((var pointer-form type &optional (mode :in) nullable)
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
          :nullable (not (null nullable))))
      (with-gensyms (pointer type-var)
        `(progv (when *handle-cycles*
                  '(*readen-values*
                    *written-values*
                    *cleaned-values*))
                (when *handle-cycles* '(() () ()))
           (let ((,pointer ,pointer-form)
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
                (if nullable
                  `(if (&? ,pointer)
                     ,expansion
                     (let ((,var void))
                       ,@body))
                  expansion))))))))

(defmacro with-values ((&rest specs) &body body)
  (if (endp specs)
    `(progn ,@body)
    `(with-value ,(first specs)
       (with-values ,(rest specs)
         ,@body))))
