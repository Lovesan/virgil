(in-package #:virgil)

(defclass proxy-type ()
  ((proxied-type
     :initform (error "Supply proxied type to proxy type")
     :initarg :type
     :accessor proxied-type)))

(defun proxy-type-p (type)
  (typep type 'proxy-type))

(defmethod base-type ((type proxy-type))
  (base-type (proxied-type type)))

(defmethod compute-alignment ((type proxy-type))
  (compute-alignment (proxied-type type)))

(defmethod lisp-type ((type proxy-type))
  (lisp-type (proxied-type type)))

(defmethod prototype ((type proxy-type))
  (prototype (proxied-type type)))

(defmethod expand-prototype ((type proxy-type))
  (expand-prototype (proxied-type type)))

(defmethod compute-size (value (type proxy-type))
  (compute-size value (proxied-type type)))

(defmethod expand-compute-size (value (type proxy-type))
  (expand-compute-size value (proxied-type type)))

(defmethod compute-fixed-size ((type proxy-type))
  (compute-fixed-size (proxied-type type)))

(defmethod compute-slot-offset (slot-name (type proxy-type))
  (compute-slot-offset slot-name (proxied-type type)))

(defmethod expand-compute-slot-offset (slot-name (type proxy-type))
  (expand-compute-slot-offset slot-name (proxied-type type)))

(defmethod convert-value (value (type proxy-type))
  (convert-value value (proxied-type type)))

(defmethod translate-value (value (type proxy-type))
  (translate-value value (proxied-type type)))

(defmethod read-value (value output (type proxy-type))
  (read-value value output (proxied-type type)))

(defmethod write-value (value pointer (type proxy-type))
  (write-value value pointer (proxied-type type)))

(defmethod allocate-value (value (type proxy-type))
  (allocate-value value (proxied-type type)))

(defmethod clean-value (pointer value (type proxy-type))
  (clean-value pointer value (proxied-type type)))

(defmethod free-value (pointer (type proxy-type))
  (free-value pointer (proxied-type type)))

(defmethod expand-convert-value (value (type proxy-type))
  (expand-convert-value value (proxied-type type)))

(defmethod expand-translate-value (value (type proxy-type))
  (expand-translate-value value (proxied-type type)))

(defmethod expand-read-value (value output (type proxy-type))
  (expand-read-value value output (proxied-type type)))

(defmethod expand-write-value (value pointer (type proxy-type))
  (expand-write-value value pointer (proxied-type type)))

(defmethod expand-allocate-value (value (type proxy-type))
  (expand-allocate-value value (proxied-type type)))

(defmethod expand-clean-value (pointer value (type proxy-type))
  (expand-clean-value pointer value (proxied-type type)))

(defmethod expand-free-value (pointer (type proxy-type))
  (expand-free-value pointer (proxied-type type)))

(defmethod expand-dynamic-extent (var value-var body (type proxy-type))
  (expand-dynamic-extent var value-var body (proxied-type type)))

(defmethod expand-callback-dynamic-extent (var value body (type proxy-type))
  (expand-callback-dynamic-extent var value body (proxied-type type)))

(defmethod expand-reference-dynamic-extent
    (var size-var value-var body mode (type proxy-type))
  (expand-reference-dynamic-extent
    var size-var value-var body mode (proxied-type type)))
