#|
 This file is a part of cl-gpio
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.gpio)

(defvar *pin-cache*
  (make-hash-table :test 'eql))

(defstruct (pin (:constructor %make-pin)
                (:copier)
                (:predicate))
  (name -1 :type integer :read-only T)
  (chip "" :type string :read-only T)
  (direction :in :type (member :in :out))
  (edge :none :type (member :none :rising :falling :both))
  (active-low NIL :type boolean))

(defmethod print-object ((pin pin) stream)
  (print-unreadable-object (pin stream :type T)
    (format NIL "~d ~s" (pin-name pin) (pin-direction pin))))

(defun make-pin (pin)
  (unless (probe-file (cl-gpio-lli:pin-file pin ""))
    (cl-gpio-lli:export-pin pin))
  (unless (probe-file (cl-gpio-lli:pin-file pin ""))
    (error "No such pin ~d." pin))
  (%make-pin :name pin
             :chip (dolist (chip (cl-gpio-lli:chips) (princ-to-string chip))
                     (when (<= 0 (- pin (cl-gpio-lli:base chip)) (cl-gpio-lli:ngpio chip))
                       (return (cl-gpio-lli:label chip))))
             :direction (cl-gpio-lli:direction pin)
             :edge (cl-gpio-lli:edge pin)
             :active-low (cl-gpio-lli:active-low pin)))

(defun ensure-pin (pin &optional refresh)
  (etypecase pin
    (pin (if refresh
             (ensure-pin (pin-name pin) T)
             pin))
    (integer
     (or (and (not refresh) (gethash pin *pin-cache*))
         (setf (gethash pin *pin-cache*)
               (make-pin pin))))))

(defun pins ()
  (mapcar #'ensure-pin (cl-gpio-lli:exported-pins)))

(defun all-pins ()
  (mapcar #'ensure-pin (cl-gpio-lli:available-pins)))

(defun export (&rest pins)
  (mapcar #'ensure-pin pins))

(defun unexport (&rest pins)
  (dolist (pin pins)
    (let ((name (etypecase pin
                  (integer pin)
                  (pin (pin-name pin)))))
      (cl-gpio-lli:unexport-pin name)
      (remhash name *pin-cache*))))

(defun name (pin)
  (pin-name (ensure-pin pin)))

(defun chip (pin)
  (pin-chip (ensure-pin pin)))

(defun direction (pin)
  (pin-direction (ensure-pin pin)))

(defun edge (pin)
  (pin-edge (ensure-pin pin)))

(defun active-low (pin)
  (pin-active-low (ensure-pin pin)))

(defun (setf direction) (direction pin)
  (let ((pin (ensure-pin pin)))
    (cond ((eql direction (pin-direction pin))
           direction)
          (T
           (setf (cl-gpio-lli:direction pin) direction)
           (setf (pin-direction pin) direction)))))

(defun (setf edge) (edge pin)
  (let ((pin (ensure-pin pin)))
    (cond ((eql edge (pin-edge pin))
           edge)
          (T
           (setf (cl-gpio-lli:edge pin) edge)
           (setf (pin-edge pin) edge)))))

(defun (setf active-low) (active-low pin)
  (let ((pin (ensure-pin pin)))
    (cond ((eql active-low (pin-active-low pin))
           active-low)
          (T
           (setf (cl-gpio-lli:active-low pin) active-low)
           (setf (pin-active-low pin) active-low)))))

(defun value (pin)
  (let ((pin (ensure-pin pin)))
    (setf (direction pin) :in)
    (cl-gpio-lli:value (pin-name pin))))

(defun (setf value) (value pin)
  (let ((pin (ensure-pin pin)))
    (setf (direction pin) :out)
    (setf (cl-gpio-lli:value (pin-name pin)) value)))

#+sbcl
(defun await-value (pin &optional timeout)
  (let ((pin (ensure-pin pin)))
    (with-open-file (fd (cl-gpio-lli:pin-file (pin-name pin) "value"))
      (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd fd) :input timeout))))

#+sbcl
(defun call-with-pin-handler (function handler pin &optional (edge NIL e-p) (active-low NIL a-p))
  (let ((pin (ensure-pin pin)))
    (with-open-file (fd (cl-gpio-lli:pin-file (pin-name pin) "value"))
      (when e-p (setf (edge pin) edge))
      (when a-p (setf (active-low pin) active-low))
      (flet ((helper ()
               (file-position fd 0)
               (funcall handler pin (cl-gpio-lli:value fd))))
        (sb-sys:with-fd-handler ((sb-sys:fd-stream-fd fd) :input #'helper)
                                (funcall function))))))

(defmacro with-pin-handler ((handler pin &optional edge active-low) &body body)
  `(call-with-pin-handler
    (lambda () ,@body)
    ,handler ,pin ,edge ,active-low))
