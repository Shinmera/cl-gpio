(in-package #:org.shirakumo.gpio.lli)

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

(in-package #:org.shirakumo.gpio)
(docs:define-docs
  (variable *pin-cache*
    "Hash table to store cached PIN instances.

See PIN
See ENSURE-PIN")

  (type pin
    "Representative type to encapsulate a GPIO pin.

This will cache current pin properties. Note that it will
not be updated automatically should changes to the pin
occur from elsewhere in the system. Only updates done
through this high-level interface will be tracked.

See MAKE-PIN
See ENSURE-PIN
See NAME
See CHIP
See DIRECTION
See EDGE
See ACTIVE-LOW
See VALUE")

  (function %make-pin
    "Direct constructor to make a PIN instance.

See PIN
See MAKE-PIN")

  (function pin-name
    "Direct reader for the pin's ID or name.

Value must be an integer.

See PIN
See NAME")

  (function pin-chip
    "Direct reader for the pin's chip (or its label).

Value must be a string.

See PIN
See CHIP")

  (function pin-direction
    "Direct accessor to the pin's I/O direction.

Value must be one of :IN :OUT

See PIN
See DIRECTION")

  (function pin-edge
    "Direct accessor to the pin's edge.

Value must be one of :NONE :RISING :FALLING :BOTH

See PIN
See EDGE")

  (function pin-active-low
    "Direct accessor to the pin's active-low.

Value must be a boolean.

See PIN
See ACTIVE-LOW")

  (function make-pin
    "Construct a new PIN instance.

If the pin has not been exported yet, it will be.
If the pin does not exist, an error is signalled.
The GPIO pin's current properties are read out and
stored into the respective slots of the pin instance.

See CL-GPIO-LLI:LABEL
See CL-GPIO-LLI:DIRECTION
See CL-GPIO-LLI:EDGE
See CL-GPIO-LLI:ACTIVE-LOW")

  (function ensure-pin
    "Ensure to get a PIN instance in return.

Accepts either a pin's name/id number or a pin instance.
If REFRESH is true, a fresh PIN instance is returned
that has its values taken from the system's GPIO values.

See *PIN-CACHE*
See MAKE-PIN
See PIN")

  (function pins
    "Returns a list of available/exported PIN instances.

See CL-GPIO-LLI:EXPORTED-PINS
See ENSURE-PIN")

  (function all-pins
    "Returns a list of PIN instances for all pins on the system.

See CL-GPIO-LLI:AVAILABLE-PINS
See ENSURE-PIN")

  (function export
    "Export the specified pins and return a list of according PIN instances.

See ENSURE-PIN
See CL-GPIO-LLI:EXPORT-PIN")

  (function unexport
    "Unexport the specified pins and invalidate their cache.

See *PIN-CACHE*
See CL-GPIO-LLI:UNEXPORT-PIN")

  (function name
    "Returns the pin's name or ID.

See PIN-NAME")

  (function chip
    "Returns the pin's chip device name.

See PIN-CHIP")

  (function direction
    "Accesses the pin's I/O direction.

If the pin does not yet exist or is not exported, it will be.
The system GPIO's value is not adjusted if the cached value
is already the same as the value attempted to be set with this.

See ENSURE-PIN
See CL-GPIO-LLI:DIRECTION
See PIN-DIRECTION")

  (function edge
    "Accesses the pin's I/O edge.

If the pin does not yet exist or is not exported, it will be.
The system GPIO's value is not adjusted if the cached value
is already the same as the value attempted to be set with this.

See ENSURE-PIN
See CL-GPIO-LLI:EDGE
See PIN-EDGE")

  (function active-low
    "Accesses the pin's I/O active-low.

If the pin does not yet exist or is not exported, it will be.
The system GPIO's value is not adjusted if the cached value
is already the same as the value attempted to be set with this.

See ENSURE-PIN
See CL-GPIO-LLI:ACTIVE-LOW
See PIN-ACTIVE-LOW")

  (function value
    "Accesses the pin's I/O value.

If the pin does not yet exist or is not exported, it will be.
The pin's I/O direction is automatically adjusted if necessary
depending on whether the value is read or set by setf.
The value returned by this is never cached.

See ENSURE-PIN
See CL-GPIO-LLI:VALUE")
  
  (function await-value
    "Wait until the pin has a value that we can read.

If TIMEOUT is specified and reached before a value becomes
accessible, NIL is returned. Otherwise, true is returned.

This function is available on the following implementations:
 * SBCL

See ENSURE-PIN")

  (function call-with-pin-handler
    "Make the HANDLER function be called if the PIN changes value during the evaluation of FUNCTION.

The HANDLER is called with the corresponding PIN instance
and the new value as arguments.

This function is available on the following implementations:
 * SBCL

See ENSURE-PIN
See EDGE
See ACTIVE-LOW")

  (function with-pin-handler
    "Shorthand to call a handler function on PIN value change during the evaluation of BODY.

See CALL-WITH-PIN-HANDLER"))
