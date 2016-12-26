## About cl-gpio
This is a bindings library for the Linux GPIO kernel module as described on <https://www.kernel.org/doc/Documentation/gpio/sysfs.txt>. It provides both primitive access and more sophisticated constructs to work with interrupts and such.

## How To
Enumerate a list of all available GPIO pins on your system:

    (gpio:available-pins)

The pins must be exported for use first:

    (gpio:export-pin 0 1 2)

You can then access the direction, value, edge, and active-low of each pin:

    (setf (gpio:direction 0) :out)
    (gpio:value 0)

On SBCL you can also wait for values:

    (progn (gpio:await-value 0)
           (format T "Whoah, 0's edge is ~a to ~:[0~;1~]" (edge 0) (value 0)))

Or even install handlers:

    (gpio:with-pin-handler (#'print 0 :falling)
      (format T "Waiting for a change on 0...")
      (loop (sleep 0.001)))

Naturally you'll have to refer to your particular board/system's specification to be able to tell which pins are supposed to be used for what. For the Raspberry Pi 2/3, it would be:

![rpi23-pin-mapping](https://az835927.vo.msecnd.net/sites/iot/Resources/images/PinMappings/RP2_Pinout.png)

([Source](https://developer.microsoft.com/en-us/windows/iot/docs/pinmappingsrpi))
