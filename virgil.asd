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

(asdf:defsystem #:virgil
  :version "0.7.4"
  :description "Virgil, a lisper's guide to the lower levels"
  :author "Dmitry Ignatiev <lovesan.ru@gmail.com>"
  :maintainer "Dmitry Ignatiev <lovesan.ru@gmail.com>"
  :licence "MIT"
  :depends-on (#:trivial-features #:cffi #:alexandria #:babel)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "features")
             (:file "base")
             (:file "proxy") 
             (:file "typedefs")
             (:file "primitives")
             (:file "pointers")
             (:file "references")
             (:file "arrays")
             (:file "strings")
             (:file "enums")
             (:file "structures")
             (:file "functions")                                      
             (:file "aligned")
             (:file "filtered")
             (:file "const")))))

(defmethod asdf:operation-done-p ((op asdf:test-op)
                                  (c (eql (asdf:find-system :virgil))))
  nil)

(defmethod asdf:perform ((op asdf:test-op)
                         (c (eql (asdf:find-system :virgil))))
  (asdf:load-system :virgil-test)
  (asdf:test-system :virgil-test))

;; vim: ft=lisp et
