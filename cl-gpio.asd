(asdf:defsystem cl-gpio
  :version "1.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
