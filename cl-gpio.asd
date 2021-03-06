#|
 This file is a part of cl-gpio
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-gpio
  :version "1.1.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library for the Linux GPIO kernel module as used on hobby kits such as the Raspberry Pi"
  :homepage "https://Shinmera.github.io/cl-gpio/"
  :bug-tracker "https://github.com/Shinmera/cl-gpio/issues"
  :source-control (:git "https://github.com/Shinmera/cl-gpio.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :cffi))
