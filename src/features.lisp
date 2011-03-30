;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2010-2011, Dmitry Ignatiev <lovesan.ru@gmail.com>

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

#-(or x86 x86-64)
(error "Unsupported platform")

(pushnew :virgil *features*)

;; see http://tkpapp.blogspot.com/2010/05/upgraded-array-element-types-and-pinned.html
;; Virgil uses babel's with-simple-vector and cffi's with-pointer-to-vector-data
#+(or sbcl cmu ecl openmcl lispworks allegro cormanlisp)
(pushnew :virgil.shareable-arrays *features*)

#+(or sbcl cmu ecl openmcl lispworks allegro cormanlisp)
(pushnew :virgil.shareable-arrays.float *features*)

#+(or sbcl cmu ecl openmcl lispworks allegro cormanlisp)
(pushnew :virgil.shareable-arrays.double *features*)

#+(or sbcl cmu ecl openmcl lispworks allegro cormanlisp)
(pushnew :virgil.shareable-arrays.int8 *features*)

#+(or sbcl cmu ecl openmcl lispworks allegro cormanlisp)
(pushnew :virgil.shareable-arrays.int16 *features*)

#+(or sbcl cmu openmcl lispworks allegro)
(pushnew :virgil.shareable-arrays.int32 *features*)

#+(and x86-64 (or sbcl cmu ecl openmcl lispworks allegro))
(pushnew :virgil.shareable-arrays.int64 *features*)
