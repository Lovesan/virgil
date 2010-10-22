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
;;; DEALINGS IN THE SOFTWARE

(in-package #:virgil)

(define-proxy-type filtered-type ()
  ((predicate :initform 'identity
              :reader filtered-type-predicate
              :initarg :predicate)
   (handler :initform 'identity
            :reader filtered-type-handler
            :initarg :handler))
  (:writer (value pointer type)
    (write-value
      (if (funcall (fdefinition (filtered-type-predicate type)) value)
        value
        (funcall (fdefinition (filtered-type-handler type)) value))
      pointer
      (proxied-type type)))
  (:writer-expansion (value-form pointer-form type)
    (with-gensyms (value pointer)
      `(let ((,value (the ,(lisp-type type) ,value-form))
             (,pointer (the pointer ,pointer-form)))
         (if (funcall #',(filtered-type-predicate type) ,value)
           ,(expand-write-value
              value
              pointer
              (proxied-type type))
           (let ((,value (funcall #',(filtered-type-handler type) ,value)))
             (declare (type ,(lisp-type type) ,value))
             ,(expand-write-value
                value
                pointer
                (proxied-type type)))))))
  (:reader (pointer out type)
    (let ((value (read-value pointer out (proxied-type type))))
      (if (funcall (fdefinition (filtered-type-predicate type)) value)
        value
        (funcall (fdefinition (filtered-type-handler type)) value))))
  (:reader-expansion (pointer-form out-form type)
    (with-gensyms (value)
      `(let ((,value ,(expand-read-value pointer-form out-form (proxied-type type))))
         (declare (type ,(lisp-type type) ,value))
         (if (funcall #',(filtered-type-predicate type) ,value)
           ,value
           (funcall #',(filtered-type-handler type) ,value)))))
  (:reference-dynamic-extent-expansion
    (var size-var value-var body mode type)
    (with-gensyms (value)
      (ecase mode
        (:in `(let ((,value ,value-var))
                (declare (type ,(lisp-type type) ,value))
                (unless (funcall #',(filtered-type-predicate type) ,value)
                  (setf ,value (funcall #',(filtered-type-handler type)
                                        ,value)))
                ,(expand-reference-dynamic-extent
                   var size-var value body mode (proxied-type type))))
        (:out `(let ((,value ,value-var))
                 (declare (type ,(lisp-type type) ,value))
                 (prog1
                  ,(expand-reference-dynamic-extent
                     var size-var value body mode (proxied-type type))
                  (if (funcall #',(filtered-type-predicate type) ,value)
                    (setf ,value (funcall #',(filtered-type-handler type)
                                          ,value)))
                  (setf ,value-var ,value))))
        (:inout `(let ((,value ,value-var))
                   (declare (type ,(lisp-type type) ,value))
                   (unless (funcall #',(filtered-type-predicate type) ,value)
                     (setf ,value (funcall #',(filtered-type-handler type)
                                           ,value)))
                   (prog1
                    ,(expand-reference-dynamic-extent
                       var size-var value body mode (proxied-type type))
                    (unless (funcall #',(filtered-type-predicate type) ,value)
                      (setf ,value (funcall #',(filtered-type-handler type)
                                            ,value)))
                    (setf ,value-var ,value))))))))

(define-immediate-type filtered-immediate-type (filtered-type)
  ()
  (:converter (value type)
    (convert-value
      (if (funcall (fdefinition (filtered-type-predicate type)) value)
        value
        (funcall (fdefinition (filtered-type-handler type)) value))
      (proxied-type type)))
  (:converter-expansion (value-form type)
    (with-gensyms (value)
      `(let ((,value (the ,(lisp-type type) ,value-form)))
         (if (funcall #',(filtered-type-predicate type) ,value)
           ,(expand-convert-value
              value
              (proxied-type type))
           (let ((,value (funcall #',(filtered-type-handler type) ,value)))
             (declare (type ,(lisp-type type) ,value))
             ,(expand-convert-value
                value
                (proxied-type type)))))))
  (:translator (value type)
    (let ((value (translate-value value (proxied-type type))))
      (if (funcall (fdefinition (filtered-type-predicate type)) value)
        value
        (funcall (fdefinition (filtered-type-handler type)) value))))
  (:translator-expansion (value-form type)
    (with-gensyms (value)
      `(let ((,value ,(expand-translate-value value-form (proxied-type type))))
         (declare (type ,(lisp-type type) ,value))
         (if (funcall #',(filtered-type-predicate type) ,value)
           ,value
           (funcall #',(filtered-type-handler type) ,value))))))

(defun valid-function-name-p (name)
  (or (symbolp name)
      (and (proper-list-p name)
           (= 2 (length name))
           (eq 'setf (first name))
           (symbolp (second name)))))

(define-type-parser filtered (type &optional (predicate 'identity)
                                             (handler   'identity))
  (assert (valid-function-name-p predicate)
      (predicate))
  (assert (valid-function-name-p handler)
      (handler))
  (let ((type (parse-typespec type)))
    (make-instance (if (immediate-type-p type)
                     'filtered-immediate-type
                     'filtered-type)
      :type type
      :predicate predicate
      :handler handler)))
      
(defmethod unparse-type ((type filtered-type))
  `(filtered ,(unparse-type (proxied-type type))
     ,(filtered-type-predicate type)
     ,(filtered-type-handler type)))
