#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.gpio)

#+sbcl
(defun await-value (pin &optional timeout)
  (with-open-file (fd (pin-file pin "value"))
    (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd fd) :input timeout)))

#+sbcl
(defun call-with-pin-handler (function handler pin &optional edge active-low)
  (with-open-file (fd (pin-file pin "value"))
    (when edge (setf (edge fd) edge))
    (when active-low (Setf (active-low fd) active-low))
    (flet ((helper ()
             (file-position fd 0)
             (funcall handler (value fd))))
      (sb-sys:with-fd-handler ((sb-sys:fd-stream-fd fd) :input #'helper)
                              (funcall function)))))

(defmacro with-pin-handler ((handler pin &optional edge active-low) &body body)
  `(call-with-pin-handler
    (lambda () ,@body)
    ,handler ,pin ,edge ,active-low))
