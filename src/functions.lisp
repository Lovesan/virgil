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

(defun reduce-to-cffi-type (type)
  (loop :with types = '()
    :for x = type :then
    (typecase x
      (primitive-type
        (return (primitive-type-cffi-type x)))
      (immediate-type
        (push (unparse-type x) types)
        (base-type x))
      (T (error "~:[~;~:*[~{~s~^ -> ~}]~%~]~s is neither primitive nor immediate type."
                (nreverse types)
                (unparse-type x))))))

(defun error-param-initform (typespec)
  (error
    "Initform should not be supplied for primary arguments: ~s"
    typespec))

(defun error-key-and-opt (types)
  (error ":KEY and :OPTIONAL parameters in the same parameter list: ~s"
         types))

(defun error-param-count (types args)
  (error "Parameter count mismatch: ~s ~s" types args))

(defun to-keyword (x)
  (intern (string x) :keyword))

(defun make-binding-form (param-types primary-args optional-args key-args)
  (loop :with primary = '()    
    :with optional = '()
    :with key = '()
    :with aux = '()
    :with nokey = (gensym)
    :for (type name cffi-type param-type initform)
    :in param-types :do
    (ecase param-type
      (:primary (push (list name (pop primary-args))
                      primary))
      (:optional (push (list name (if (null optional-args)
                                    initform
                                    (pop optional-args)))
                       optional))
      (:key (push (list name (let ((arg (getf key-args
                                              (to-keyword name)
                                              nokey)))
                               (if (eq arg nokey) initform arg)))
                  key))
      (:aux (push (list name initform) aux)))
    :finally (return
               (nreconc primary
                        (nreconc optional
                                 (nreconc key
                                          (nreverse aux)))))))

(defun duplicate-parameters-exist-p (param-types)
  (loop :with names = '()
    :for (type name) :in param-types
    :do (if (find name names)
          (return t)
          (push name names))))

(defmacro assert-valid-arg-name (name)
  `(assert (and (symbolp ,name)
                (not (constantp ,name)))
       (,name)
     "Parameter name must be valid symbol: ~s" ,name))

(defun parse-args-and-types (args types)
  (loop :with primary-count = 0
    :with optional-count = 0
    :with key-count = 0
    :for type-form :in types :collect
    (let* ((typespec (car type-form))
           (type (parse-typespec typespec)))
      (destructuring-bind
          (&optional (name (gensym))
           (param-type :primary)
           (initform
             (case param-type
               ((:aux :optional :key) (expand-prototype type)))
             initform-p))
          (rest type-form)
        (assert-valid-arg-name name)
        (ecase param-type
          (:primary (if initform-p
                      (error-param-initform typespec)
                      (incf primary-count)))
          (:optional (if (> key-count 0)
                       (error-key-and-opt types)
                       (incf optional-count)))
          (:key (if (> optional-count 0)
                  (error-key-and-opt types)
                  (incf key-count)))
          (:aux t))
        (list type name (reduce-to-cffi-type type)
              param-type initform)))
    :into param-types
    :finally (let* ((arg-count (length args))
                    (primary-args (if (< arg-count primary-count)
                                    (error-param-count types args)
                                    (subseq args 0 primary-count)))
                    (optional-args (unless (> key-count 0)
                                     (if (> arg-count (+ primary-count optional-count))
                                       (error-param-count types args)
                                       (subseq args primary-count))))
                    (key-args (unless (> optional-count 0)
                                (cond
                                  ((> arg-count (+ primary-count (* 2 key-count)))
                                   (error-param-count types args))
                                  ((not (evenp (- arg-count primary-count)))
                                   (error "Odd number of keyword arguments: ~s" args))
                                  (T (loop :with keys = (subseq args primary-count)
                                       :for (k v) :on keys :by #'cddr :do
                                       (unless (keywordp k)
                                         (error "Invalid keyword name: ~s" k))
                                       (unless (find k param-types
                                                     :key (lambda (x) (to-keyword (second x))))
                                         (error "Unknown keyword name: ~s" k))
                                       :finally (return keys)))))))
               (when (duplicate-parameters-exist-p param-types)
                 (error "Duplicate parameter names: ~s" types))
               (return
                 (values param-types
                         (make-binding-form param-types 
                           primary-args
                           optional-args
                           key-args))))))

(defun make-cffi-call-form (call-form param-types retname rettype)
  (labels ((iter (param-types cffi-args)
             (if (endp param-types)
               (with-gensyms (return-value)
                 `(let ((,return-value (,@call-form
                                        ,@cffi-args
                                        ,(reduce-to-cffi-type rettype))))
                    (declare (ignorable ,return-value))
                    (setf ,retname ,(expand-translate-value
                                      return-value rettype))))
               (destructuring-bind
                   (type name cffi-type &rest rest) (first param-types)
                 (declare (ignore rest))
                 (with-gensyms (cffi-value-var)
                   (expand-dynamic-extent
                     cffi-value-var
                     name
                     (list (iter (rest param-types)
                                 (nconc cffi-args
                                        (list cffi-type
                                              cffi-value-var))))
                     type))))))
    (iter param-types '())))

(defun collect-type-decls (arg-types &optional (aux t))
  (loop :for (type name cffi-type arg-type)
    :in arg-types
    :when (or aux (and (not aux)
                       (not (eq arg-type :aux))))
    :collect `(type ,(lisp-type type) ,name)))

(defun make-external-call-body
    (call-form arg-types binding-form result-form return-value-name rettype)
  `(let* (,@binding-form (,return-value-name ,(expand-prototype rettype)))
     (declare (type ,(lisp-type rettype) ,return-value-name)
              ,@(collect-type-decls arg-types))
     ,(make-cffi-call-form
        call-form  
        arg-types
        return-value-name rettype)
     ,result-form))

(defmacro external-pointer-call
    (pointer ((&optional (convention :cdecl))
              (return-type &optional (return-value-name (gensym))
                           (result-form return-value-name))
              &rest arg-types)
             &rest args)
  (check-type convention (member :cdecl :stdcall))
  (multiple-value-call
    #'make-external-call-body
    `(foreign-funcall-pointer ,pointer (:convention ,convention))
    (parse-args-and-types args arg-types)
    result-form
    return-value-name
    (parse-typespec return-type)))

(defmacro external-function-call
    (name ((&optional (convention :cdecl) (library :default))
           (return-type &optional (return-value-name (gensym))
                        (result-form return-value-name))
           &rest arg-types)
          &rest args)
  (check-type convention (member :cdecl :stdcall))
  (multiple-value-call
    #'make-external-call-body
    `(foreign-funcall (,name :convention ,convention :library ,library))
    (parse-args-and-types args arg-types)
    result-form
    return-value-name
    (parse-typespec return-type)))

(defun write-char-with-case (char out case)
  (write-char (case case
                (:downcase (char-downcase char))
                (:upcase (char-upcase char))
                (:invert (if (lower-case-p char)
                           (char-upcase char)
                           (char-downcase char)))
                (T char))
              out))

(defgeneric translate-name (external-name translation-type &rest args)
  (:method (name type &rest args)
           (declare (ignore name args))
    (error "Unknown name translation type: ~s" type))
  (:method (name (type (eql :bare)) &rest args)
    (destructuring-bind
        (&optional (case (readtable-case *readtable*))) args
      (intern (with-output-to-string (out)
                (loop :for c :across (the string name)
                  :do (write-char-with-case c out case))))))
  (:method (name (type (eql :snake-case)) &rest args)
    (destructuring-bind
        (&optional (case (readtable-case *readtable*))) args      
      (intern (with-output-to-string (out)
                (loop
                  :for c :of-type character :across (the string name) :do
                  (if (char= c #\_)
                    (write-char #\- out)
                    (write-char-with-case c out case)))))))
  (:method (name (type (eql :camel-case)) &rest args)
    (destructuring-bind
        (&optional (case (readtable-case *readtable*))) args
      (intern (with-output-to-string (out)
                (loop :with dc = nil
                  :for c :of-type character :across (the string name) :do
                  (if (lower-case-p c)
                    (setf dc t)
                    (when dc (setf dc nil) (write-char #\- out)))
                  (write-char-with-case c out case)))))))

(defun parse-function-name-spec (spec)
  (etypecase spec
    (string (values spec (translate-name spec :snake-case)))
    (cons (destructuring-bind
              (external-name &optional (lisp-name '(:snake-case))) spec
            (check-type external-name string)
            (etypecase lisp-name
              (symbol (values external-name lisp-name))
              (cons (values external-name
                            (apply #'translate-name
                                   external-name lisp-name))))))))

(defun revappend* (&rest lists)
  (reduce #'append (mapcar #'reverse lists)))

(defun arg-specs->args (arg-types)
  (loop :with primary = '()
    :with optional = '()
    :with key = '()
    :with aux = ()
    :with normalized = '()
    :with rest = nil
    :for (type-form . rest-types) :on arg-types :do
    (if (eq type-form '&rest)
      (progn (assert (null rest-types)
                 ()
               "&rest, if supplied, must appear at the end of the argument list: ~s"
               arg-types)
             (setf rest t)
             (loop-finish))
      (destructuring-bind
          (name type &optional (arg-type :primary)
                (initform nil initform-p))
          type-form
        (push (list* type
                     name
                     arg-type
                     (when initform-p
                       (list initform)))
              normalized)
        (ecase arg-type
          (:primary (push name primary))
          (:optional (push name optional))
          (:key (push (to-keyword name) key)
                (push name key))
          (:aux (push name aux)))))
    :finally (return (values (nreverse normalized)
                             (revappend* primary optional key)
                             (nreverse primary)
                             (nreverse optional)
                             (loop :for (k n) :on (nreverse key) :by #'cddr
                               :collect n)
                             (nreverse aux)
                             rest))))

(defun make-lisp-ext-args (arg-group-name arg-group arg-types)
  (unless (null arg-group)
    (list* arg-group-name      
      (loop :for name :in arg-group
        :collect `(,name ,(fifth (find name arg-types
                                       :key #'second)))))))

(defmacro define-external-function
    (name
     (&optional (convention :cdecl) (library :default))
     (return-type &optional (return-value-name (gensym))
                  (result-form return-value-name))
     &body doc-and-args)
  (multiple-value-bind
      (external-name lisp-name) (parse-function-name-spec name)
    (let* ((doc (if (stringp (car doc-and-args))
                  (car doc-and-args)                  
                  nil))
           (arguments (if (stringp (car doc-and-args))
                        (rest doc-and-args)                        
                        doc-and-args)))
      (multiple-value-bind
          (normalized-types args primary optional key aux rest)
          (arg-specs->args arguments)
        (declare (ignore aux))
        (if rest
          `(defmacro ,lisp-name (((&optional (result-form ',result-form))
                                  &rest arg-types)
                                 &rest args)
             ,doc
             `(external-function-call                
                ,',external-name
                ((,',convention ,',library)
                 (,',return-type ,',return-value-name ,result-form)
                 ,@',normalized-types
                 ,@arg-types)
                ,@args))
          (multiple-value-bind
              (arg-types binding-form)
              (parse-args-and-types args normalized-types)
            `(defun ,lisp-name ,(append primary
                                        (make-lisp-ext-args '&optional optional arg-types)
                                        (make-lisp-ext-args '&key key arg-types))
               (declare ,@(collect-type-decls arg-types nil))
               ,doc
               ,(make-external-call-body
                  `(foreign-funcall
                       (,external-name :convention ,convention
                                       :library ,library))
                  arg-types
                  binding-form
                  result-form
                  return-value-name
                  (parse-typespec return-type)))))))))


(defun make-callback-body (arg-types body)
  (labels ((iter (arg-types)
             (if (null arg-types)
               `(locally ,@body)
               (destructuring-bind
                   (type name cffi-type) (car arg-types)
                 (declare (ignore cffi-type))
                 (expand-callback-dynamic-extent
                   name
                   name
                   (list (iter (rest arg-types)))
                   type)))))
    (iter arg-types)))

(defmacro define-callback (name-and-options
                            return-type
                            (&rest args)
                            &body body)
  (destructuring-bind
      (name &optional (convention :cdecl))
      (ensure-list name-and-options)
    (check-type convention (member :cdecl :stdcall))
    (let ((rettype (parse-typespec return-type))
          (arg-types (loop :for (arg-name arg-type) :in args
                       :for type = (parse-typespec arg-type) :collect
                       (progn (assert-valid-arg-name arg-name)
                              (list type arg-name (reduce-to-cffi-type type))))))      
      `(defcallback (,name :convention ,convention)
           ,(reduce-to-cffi-type rettype)
         ,(mapcar (lambda (x) (last x 2)) arg-types)
         ,(with-gensyms (whole)
            `(let ((,whole (block ,name ,(make-callback-body arg-types body))))
               (declare (type ,(lisp-type rettype) ,whole)
                        (ignorable ,whole))
               ,(expand-convert-value whole rettype)))))))
