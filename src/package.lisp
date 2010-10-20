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

(in-package :cl-user)

(defpackage #:virgil
  (:use #:cl #:cffi #:alexandria #:babel #:babel-encodings)
  (:export    
    ;;typespec stuff
    #:parse-typespec
    #:unparse-type
    
    ;;core generics:
    #:base-type
    #:lisp-type
    #:prototype
    #:expand-prototype
    #:compute-size
    #:expand-compute-size
    #:compute-fixed-size
    #:compute-alignment
    #:compute-slot-offset
    #:expand-compute-slot-offset
    #:translate-value
    #:convert-value
    #:read-value
    #:write-value
    #:clean-value
    #:allocate-value
    #:free-value
    #:expand-translate-value
    #:expand-convert-value
    #:expand-read-value
    #:expand-write-value
    #:expand-clean-value
    #:expand-allocate-value
    #:expand-free-value
    #:expand-dynamic-extent
    #:expand-reference-dynamic-extent
    #:expand-callback-dynamic-extent
    
    ;;user-level analogues
    #:sizeof
    #:alignof
    #:offsetof
    #:convert
    #:translate
    #:alloc
    #:free
    #:clean
    #:clean-and-free
    
    ;;proxy type stuff
    #:proxied-type
    
    ;;define new types with this
    #:define-type-parser
    #:defalias
    #:define-primitive-type
    #:define-immediate-type
    #:define-aggregate-type
    #:define-proxy-type
    
    ;;type predicates
    #:primitive-type-p
    #:immediate-type-p
    #:aggregate-type-p
    #:proxy-type-p
    
    ;;primitive types
    #:char-t
    #:uchar-t
    #:wchar-t
    #:sbyte
    #:ubyte
    #:byte
    #:short
    #:ushort
    #:int
    #:uint
    #:long
    #:ulong
    #:llong
    #:ullong
    #:int-ptr
    #:uint-ptr
    #:int8
    #:uint8
    #:int16
    #:uint16
    #:int32
    #:uint32
    #:int64
    #:uint64
    #:size-t
    #:ssize-t
    #:ptrdiff-t
    #:single
    #:float
    #:double
    #:bool
    #:boolean
    #:char
    #:wchar
    
    ;;void stuff
    #:void
    #:voidp
    
    ;;pointer and reference stuff
    #:pointer
    #:*
    #:&
    #:&&
    #:&p
    #:&=
    #:&+
    #:&-
    #:&?
    #:&0    
    #:deref
    #:with-reference
    #:with-references
    #:with-pointer
    #:with-pointers
    #:with-value
    #:with-values
    
    ;;sequence and array stuff
    #:sequence
    #:~
    #:array
    #:simple-array
    
    ;;string stuff
    #:string
    #:cstring-size
    #:read-cstring
    #:write-cstring
    #:allocate-cstring
    
    ;;enum stuff
    #:enum
    #:define-enum
    
    ;;struct stuff
    #:struct
    #:offsetof
    #:define-struct
    #:union
    #:define-union
    
    ;;functions and so on
    #:external-pointer-call
    #:external-function-call
    #:translate-name
    #:define-external-function
    #:define-callback
        
    ;;aligned type
    #:aligned
    
    ;;filtered type
    #:filtered
    
    ;;also export CFFI's stuff
    #:callback
    #:get-callback
    #:define-foreign-library
    #:load-foreign-library
    #:close-foreign-library
    #:use-foreign-library
    #:load-foreign-library-error
    #:*foreign-library-directories*
    #:*darwin-framework-directories*
    ))
