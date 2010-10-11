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

(declaim (inline swap-u16))
(defun swap-u16 (x)
  (declare (type uint16 x))
  (logand #xFFFF (logior
                   (ash (logand #xFF00 x) -8)
                   (ash (logand #x00FF x) 8))))

(declaim (inline swap-u32))
(defun swap-u32 (x)
  (declare (type uint32 x))
  (logand #xFFFFFFFF
          (logior (ash (logand #xFF000000 x) -24)
                  (ash (logand #x00FF0000 x) -8)
                  (ash (logand #x0000FF00 x) 8)
                  (ash (logand #x000000FF x) 24))))

;;adapted from CFFI
(defmacro mref (ptr offs &optional (bytes 1) (endianness :ne))
  (check-type endianness (member :be :le :ne :re))
  (let ((be (member endianness '(:be #+little-endian :re
                                 #+big-endian :ne))))
    (ecase bytes
      (1 `(deref ,ptr 'uint8 ,offs))
      (2 (let ((form `(deref ,ptr 'uint16 ,offs)))
           (if be
             #+big-endian form
             #+little-endian `(swap-u16 ,form)
             #+big-endian `(swap-u16 ,form)
             #+little-endian form)))
      (4 (let ((form `(deref ,ptr 'uint32 ,offs)))
           (if be
             #+big-endian form
             #+little-endian `(swap-u32 ,form)
             #+big-endian `(swap-u32 ,form)
             #+little-endian form))))))

(defmacro mset (value ptr offs &optional (bytes 1) (endianness :ne))
  (check-type endianness (member :be :le :ne :re))
  (let ((be (member endianness '(:be #+little-endian :re
                                 #+big-endian :ne))))
    (ecase bytes
      (1 `(setf (deref ,ptr 'uint8 ,offs) ,value))
      (2 `(setf (deref ,ptr 'uint16 ,offs)
                ,(if be
                   #+big-endian value
                   #+little-endian `(swap-u16 ,value)
                   #+big-endian `(swap-u16 ,value)
                   #+little-endian value)))
      (4 `(setf (deref ,ptr 'uint32 ,offs)
                ,(if be
                   #+big-endian value
                   #+little-endian `(swap-u32 ,value)
                   #+big-endian `(swap-u32 ,value)
                   #+little-endian value))))))

(defmacro strref (string index)
  `(char-code (schar ,string ,index)))

(defmacro strset (code string index)
  `(setf (schar ,string ,index) (code-char ,code)))

(defparameter *string-pointer-mappings*
    (instantiate-concrete-mappings
        :octet-seq-setter mset
      :octet-seq-getter mref
      :octet-seq-type pointer
      :code-point-seq-setter strset
      :code-point-seq-getter strref
      :code-point-seq-type simple-unicode-string))

(defun cstring-length (pointer &key (encoding :ascii) (offset 0))
  (declare (type pointer pointer)
           (type non-negative-fixnum offset))
  (macrolet ((strlen (p offs size)
               `(loop :for i :of-type non-negative-fixnum :from 0 :by ,size
                  :until (zerop
                           ,(ecase size
                              (1 `(deref ,p 'uint8 (+ i ,offs)))
                              (2 `(deref ,p 'uint16 (+ i ,offs)))
                              (4 `(deref ,p 'uint32 (+ i ,offs)))))
                  :finally (return i))))
    (ecase (length (enc-nul-encoding
                     (get-character-encoding encoding)))
      (1 (strlen pointer offset 1))
      (2 (strlen pointer offset 2))
      (4 (strlen pointer offset 4)))))

(defun read-cstring (pointer &key (offset 0)
                             (encoding :ascii)
                             byte-length
                             out
                             (start 0)
                             end)
  (declare (type pointer pointer)
           (type non-negative-fixnum offset start)
           (type (or null string) out)
           (type (or null non-negative-fixnum) byte-length))
  (let* ((encoding (get-character-encoding encoding))
         (mapping (lookup-mapping *string-pointer-mappings* encoding))
         (max-bytes (or byte-length
                        (cstring-length pointer :encoding encoding
                                        :offset offset)))
         (max-chars (if (null out)
                      (funcall (code-point-counter mapping)
                               pointer offset (+ offset max-bytes)
                               (1- array-total-size-limit))
                      (- (or end (length out))
                         start)))
         (result (or out (make-string max-chars :element-type 'character))))
    (with-checked-simple-vector ((result result)
                                 (start (if (null out) 0 start))
                                 (end (if (null out) nil end)))
      (declare (ignore end))
      (funcall (decoder mapping)
               pointer offset (+ offset max-bytes)
               result start))
    result))

(declaim (inline bom-vector))
(defun bom-vector (encoding)
  (coerce
    (if (enc-use-bom encoding)
      (enc-bom-encoding encoding)
      #())
    '(simple-array (unsigned-byte 8) (*))))

(defun cstring-size (string &key (encoding :ascii) (start 0) end)
  (declare (type non-negative-fixnum start)
           (type (or null non-negative-fixnum) end))
  (let ((encoding (get-character-encoding encoding)))
    (with-checked-simple-vector ((string string) (start start) (end end))
      (declare (type simple-string string)
               (type non-negative-fixnum start end))
      (values (funcall (octet-counter
                         (lookup-mapping *string-pointer-mappings* encoding))
                       string start end -1)
              (if (enc-use-bom encoding)
                (length (enc-bom-encoding encoding))
                0)
              (length (enc-nul-encoding encoding))))))

(defun write-cstring (string pointer &key (start 0)
                             end
                             (offset 0)
                             (encoding :ascii)
                             byte-length)
  (declare (type string string)
           (type pointer pointer)
           (type non-negative-fixnum start offset)
           (type (or null non-negative-fixnum) end byte-length))
  (with-checked-simple-vector ((string string) (start start) (end end))
    (declare (type simple-string string)
             (type non-negative-fixnum start end))
    (setf pointer (inc-pointer pointer offset))
    (let* ((encoding (get-character-encoding encoding))
           (mapping (lookup-mapping *string-pointer-mappings* encoding))
           (bom-vector (bom-vector encoding)))
      (declare (type (simple-array uint8 (*)) bom-vector))
      (multiple-value-bind
          (data-len bom-len nt-len)
          (cstring-size string :encoding encoding :start start :end end)
        (declare (type non-negative-fixnum data-len bom-len nt-len))
        (multiple-value-bind
            (data-len end)
            (if (null byte-length)
              (values data-len end)
              (let ((data-len (min data-len (- byte-length bom-len))))
                (cond
                  ((minusp data-len) (setf bom-len 0) (values 0 start))
                  ((zerop data-len) (values 0 start))
                  (T (funcall (octet-counter mapping)
                              string start end data-len)))))
          (declare (type non-negative-fixnum data-len end))
          (dotimes (i bom-len)
            (setf (deref pointer 'uint8 i)
                  (aref bom-vector i)))
          (funcall (encoder mapping)
                   string start end pointer bom-len)
          (when (null byte-length)
            (dotimes (i nt-len)
              (setf (deref pointer 'uint8 (+ bom-len data-len i)) 0)))
          pointer)))))

(defun allocate-cstring (string &key (encoding :ascii)
                                (start 0)
                                end
                                (null-terminated-p t))
  (declare (type string string)
           (type non-negative-fixnum start)
           (type (or null non-negative-fixnum) end))
  (with-checked-simple-vector ((string string) (start start) (end end))
    (declare (type simple-string string)
             (type non-negative-fixnum start end))
    (let* ((encoding (get-character-encoding encoding))
           (mapping (lookup-mapping *string-pointer-mappings* encoding)))
      (multiple-value-bind
          (data-len bom-len nt-len)
          (cstring-size string :encoding encoding :start start :end end)
        (declare (type non-negative-fixnum data-len bom-len nt-len))
        (let ((pointer (foreign-alloc :uint8 :count (+ data-len
                                                       bom-len
                                                       (if null-terminated-p
                                                         nt-len
                                                         0))))
              (bom-vector (bom-vector encoding)))
          (declare (type pointer pointer)
                   (type (simple-array uint8 (*)) bom-vector))
          (handler-case
              (progn
                (setf (deref pointer '(simple-array uint8)) bom-vector)
                (funcall (encoder mapping)
                         string start end pointer bom-len)
                (when null-terminated-p
                  (dotimes (i nt-len)
                    (setf (deref pointer 'uint8 (+ bom-len data-len i)) 0)))
                pointer)
            (error (e) (foreign-free (the pointer pointer)) (error e))))))))

(define-aggregate-type string-type ()
  ((encoding :initform :encoding
             :initarg :encoding
             :reader strtype-encoding))
  (:size (value type)
    (multiple-value-bind
        (data-len bom-len nt-len)
        (cstring-size value :encoding (strtype-encoding type))
      (declare (type non-negative-fixnum data-len bom-len nt-len))
      (+ data-len bom-len nt-len)))
  (:size-expansion (value-form type)
    (with-gensyms (data-len bom-len nt-len)
      `(multiple-value-bind
           (,data-len ,bom-len ,nt-len)
           (cstring-size ,value-form :encoding ,(strtype-encoding type))
         (declare (type non-negative-fixnum ,data-len ,bom-len ,nt-len))
         (+ ,data-len ,bom-len ,nt-len))))
  (:align (type)
    ;;this should be enough for x86 platform
    (length (enc-nul-encoding
              (get-character-encoding (strtype-encoding type)))))
  (:lisp-type (type) 'string)
  (:prototype (type) "")
  (:prototype-expansion (type) "")
  (:reader (pointer out type)
    (read-cstring pointer :out out :encoding (strtype-encoding type)))
  (:writer (value pointer type)
    (write-cstring value pointer :encoding (strtype-encoding type)))
  (:cleaner (pointer value type) nil)
  (:reader-expansion (pointer-form out-form type)
    `(read-cstring ,pointer-form :out ,out-form
                   :encoding ,(strtype-encoding type)))
  (:writer-expansion (value-form pointer-form type)
    `(write-cstring ,value-form ,pointer-form
                    :encoding ,(strtype-encoding type)))
  (:cleaner-expansion (pointer-form value-form type) nil))

(define-aggregate-type static-string-type (string-type)
  ((byte-length :initarg :byte-length
                :initform 0
                :reader strtype-byte-length))
  (:fixed-size (type) (strtype-byte-length type))
  (:prototype (type)
    (make-string
        (floor (strtype-byte-length type)
               (length (enc-nul-encoding
                         (get-character-encoding
                           (strtype-encoding type)))))))
  (:prototype-expansion (type)
    `(make-string
         ,(floor (strtype-byte-length type)
                 (length (enc-nul-encoding
                           (get-character-encoding
                             (strtype-encoding type)))))))
  (:reader (pointer out type)
    (read-cstring pointer :out out :encoding (strtype-encoding type)
                  :byte-length (strtype-byte-length type)))
  (:writer (value pointer type)
    (write-cstring value pointer :encoding (strtype-encoding type)
                   :byte-length (strtype-byte-length type)))
  (:reader (pointer out type)
    `(read-cstring ,pointer :out ,out :encoding ,(strtype-encoding type)
                   :byte-length ,(strtype-byte-length type)))
  (:writer (value pointer type)
    `(write-cstring ,value ,pointer :encoding ,(strtype-encoding type)
                    :byte-length ,(strtype-byte-length type))))

(define-type-parser string (&key (encoding :ascii) byte-length)
  (check-type encoding keyword)
  (check-type byte-length (or null non-negative-fixnum))
  (if (null byte-length)
    (make-instance 'string-type :encoding encoding)
    (make-instance 'static-string-type
      :encoding encoding
      :byte-length byte-length)))

(defmethod unparse-type ((type string-type))
  `(string :encoding ,(strtype-encoding type)))

(defmethod unparse-type ((type static-string-type))
  `(string :encoding ,(strtype-encoding type)
           :byte-length ,(strtype-byte-length type)))
