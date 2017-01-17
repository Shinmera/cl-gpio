#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:cl-gpio
  (:nicknames #:org.shirakumo.gpio #:gpio)
  (:use #:cl)
  ;; low-level.lisp
  (:export
   #:*gpio-root*
   #:gpio-file
   #:pin-file
   #:chip-file
   #:export-pin
   #:unexport-pin
   #:exported-pins
   #:chips
   #:base
   #:label
   #:ngpio
   #:chip-pins
   #:available-pins
   #:direction
   #:value
   #:edge
   #:active-low)
  ;; wrapper.lisp
  (:export
   #:await-value
   #:call-with-pin-handler
   #:with-pin-handler))
