#|
 This file is a part of cl-gpio
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

;; See https://www.kernel.org/doc/Documentation/gpio/sysfs.txt

(in-package #:org.shirakumo.gpio.lli)

(defvar *gpio-root* "/sys/class/gpio/")

(defun directory-name (pathname)
  (let ((directory (pathname-directory pathname)))
    (when (cdr directory)
      (car (last directory)))))

(defun gpio-file (sub)
  (let ((string (make-string (+ (length *gpio-root*) (length sub)) :element-type 'base-char)))
    (replace string *gpio-root*)
    (replace string sub :start1 (length *gpio-root*))))

(defun pin-file (pin sub)
  (gpio-file (format NIL "gpio~a/~a" pin sub)))

(defun chip-file (chip sub)
  (gpio-file (format NIL "gpiochip~a/~a" chip sub)))

(cffi:defcfun (copen "open") :int
  (filename :string)
  (mode :int))

(cffi:defcfun (cclose "close") :int
  (stream :int))

(cffi:defcfun (cwrite "write") :int
  (stream :int)
  (buffer :string)
  (length :uint))

(cffi:defcfun (cread "read") :int
  (stream :int)
  (buffer :string)
  (length :uint))

(defun write-to-file (sequence file)
  (declare (type simple-string sequence file))
  (declare (optimize speed))
  (let ((fd (copen file 1)))
    (print (cwrite fd sequence (length sequence)))
    (cclose fd)))

(defun read-from-file (file)
  (declare (type simple-string file))
  (declare (optimize speed))
  (let ((fd (copen file 0)))
    (unwind-protect
         (cffi:with-foreign-object (buffer :uchar 8)
           (cffi:foreign-string-to-lisp buffer :count (1- (the (signed-byte 32) (cread fd buffer 8)))))
      (cclose fd))))

(defun export-pin (&rest pins)
  (dolist (pin pins)
    (write-to-file (princ-to-string pin) (gpio-file "export"))
    (sleep 1)))

(defun unexport-pin (&rest pins)
  (dolist (pin pins)
    (write-to-file (princ-to-string pin) (gpio-file "unexport"))))

(defun exported-pins ()
  (loop for directory in (directory (gpio-file "*/"))
        for name = (directory-name directory)
        unless (search "gpiochip" name)
        collect (parse-integer (subseq name (length "gpio")))))

(defun chips ()
  (loop for directory in (directory (gpio-file "*/"))
        for name = (directory-name directory)
        when (search "gpiochip" name)
        collect (parse-integer (subseq name (length "gpiochip")))))

(defun base (chip)
  (parse-integer (read-from-file (chip-file chip "base"))))

(defun label (chip)
  (read-from-file (chip-file chip "label")))

(defun ngpio (chip)
  (parse-integer (read-from-file (chip-file chip "ngpio"))))

(defun chip-pins (chip)
  (loop for i from (base chip)
        repeat (ngpio chip)
        collect i))

(defun available-pins ()
  (sort (loop for chip in (chips)
              append (chip-pins chip))
        #'<))

(defmacro define-pin-accessor (name file &rest values)
  `(progn
     (defun ,name (pin)
       (let ((value (read-from-file (typecase pin
                                      (stream pin)
                                      (T (pin-file pin ,file))))))
         (cond ,@(loop for (string value) in values
                       collect `((equal value ,string) ,value))
               (T (error "Unexpected value in GPIO file: ~s" value)))))
     (defun (setf ,name) (value pin)
       (write-to-file
        (ecase value
          ,@(loop for (string value) in values
                  collect `((,value) ,string)))
        (typecase pin
          (stream pin)
          (T (pin-file pin ,file))))
       value)))

(define-pin-accessor direction "direction"
  ("in" :in)
  ("out" :out))

(define-pin-accessor value "value"
  ("0" NIL)
  ("1" T))

(define-pin-accessor edge "edge"
  ("none" :none)
  ("rising" :rising)
  ("falling" :falling)
  ("both" :both))

(define-pin-accessor active-low "active_low"
  ("0" NIL)
  ("1" T))
