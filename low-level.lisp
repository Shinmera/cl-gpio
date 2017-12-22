#|
 This file is a part of cl-gpio
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

;; See https://www.kernel.org/doc/Documentation/gpio/sysfs.txt

(in-package #:org.shirakumo.gpio.lli)

(defvar *gpio-root* #p"/sys/class/gpio/")

(defun directory-name (pathname)
  (let ((directory (pathname-directory pathname)))
    (when (cdr directory)
      (car (last directory)))))

(defun gpio-file (sub)
  (merge-pathnames sub *gpio-root*))

(defun pin-file (pin sub)
  (gpio-file (format NIL "gpio~a/~a" pin sub)))

(defun chip-file (chip sub)
  (gpio-file (format NIL "gpiochip~a/~a" chip sub)))

(defun write-to-file (sequence file)
  (etypecase file
    (string
     (write-to-file sequence (pathname file)))
    (pathname
     (with-open-file (stream file :direction :output
                                  :if-exists :overwrite
                                  :element-type 'base-char)
       (write-to-file sequence stream)))
    (stream
     (write-sequence sequence file))))

(defun read-from-file (file)
  (etypecase file
    (string (read-from-file (pathname file)))
    (pathname
     (with-open-file (stream file :direction :input
                                  :if-does-not-exist :error
                                  :element-type 'base-char)
       (read-from-file stream)))
    (stream
     (string-trim '(#\Return #\Linefeed #\Space)
      (with-output-to-string (out NIL :element-type 'base-char)
        (let ((buffer (make-array 64 :element-type 'base-char)))
          (loop for size = (read-sequence buffer file)
                until (= 0 size)
                do (write-sequence buffer out :end size))))))))

(defun export-pin (&rest pins)
  (dolist (pin pins)
    (write-to-file (princ-to-string pin) (gpio-file "export"))))

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
