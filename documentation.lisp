#|
 This file is a part of cl-gamepad
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.gpio)

;; low-level.lisp
(docs:define-docs
  (variable *gpio-root*
    "The root directory of the GPIO system devices.
Should be /sys/class/gpio/")

  (function directory-name
    "Returns the name of the topmost directory, if any.")

  (function gpio-file
    "Returns an absolute path to the requested gpio file.

See *GPIO-ROOT*")

  (function pin-file
    "Returns a file for the specified GPIO pin.

See GPIO-FILE")

  (function chip-file
    "Returns a file for the specified GPIO chip.

See GPIO-FILE")

  (function write-to-file
    "Write the sequence to the specified file.")

  (function read-from-file
    "Read the file into a string, trimmed from whitespace.")

  (function export-pin
    "Export the specified pins so that they may be accessed from userspace.")

  (function unexport-pin
    "Unexport the specified pins so that they can no longer be accessed from userspace.")

  (function exported-pins
    "Returns a list of GPIO pins that are accessible.")

  (function chips
    "Returns a list of known GPIO chips.")

  (function base
    "Return the GPIO chip's base pin number.")

  (function label
    "Return the GPIO chip's label.")

  (function ngpio
    "Return the number of GPIO pins on the chip.")

  (function chip-pins
    "Return a list of GPIO pin numbers on the chip.")

  (function available-pins
    "Return a list of all available GPIO pins on the system.

Note that the pins are not necessarily accessible;
they may need to be exported first.")

  (function define-pin-accessor
    "Wrapper to define an accessor function for a GPIO pin property.")

  (function direction
    "Accessor to the GPIO pin's direction.

The value should be either :IN or :OUT.")

  (function value
    "Accessor to the GPIO pin's value.

When reading, the pin's direction should be :IN
When setting, the pin's direction should be :OUT

The value should be either NIL or T.")

  (function edge
    "Accessor to the GPIO pin's interrupt edge.

The value should be one of :NONE :RISING :FALLING :BOTH")

  (function active-low
    "Accessor to whether the GPIO pin has an active low.

The value should be either NIL or T."))

(docs:define-docs
  (function await-value
    "Wait until the pin has a value that we can read.

If TIMEOUT is specified and reached before a value becomes
accessible, NIL is returned. Otherwise, true is returned.

This function is available on the following implementations:
 * SBCL")

  (function call-with-pin-handler
    "Make the HANDLER function be called if the PIN changes value during the evaluation of FUNCTION.

This function is available on the following implementations:
 * SBCL

See EDGE
See ACTIVE-LOW")

  (function with-pin-handler
    "Shorthand to call a handler function on PIN value change during the evaluation of BODY.
g
See CALL-WITH-PIN-HANDLER"))
