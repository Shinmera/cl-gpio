## About cl-gpio
This is a bindings library for the Linux GPIO kernel module as described on <https://www.kernel.org/doc/Documentation/gpio/sysfs.txt>. It provides both primitive access and more sophisticated constructs to work with interrupts and such.

## How To
Enumerate a list of all available GPIO pins on your system:

    (gpio:pins)

If you have GPIO pins on your system and GPIO:PINS returns NIL, then try to export them first:

	;; Replace 26 with how many pins you have:
	(loop for i from 0 to 26
		  do (gpio:export i))

GPIO:PINS should now return the pins' statuses:

	(gpio:pins) =>

	(#<CL-GPIO:PIN 0 :OUT> #<CL-GPIO:PIN 1 :IN> #<CL-GPIO:PIN 10 :IN>
	#<CL-GPIO:PIN 11 :IN> #<CL-GPIO:PIN 12 :IN> #<CL-GPIO:PIN 13 :IN>
	#<CL-GPIO:PIN 14 :IN> #<CL-GPIO:PIN 15 :IN> #<CL-GPIO:PIN 16 :IN>
	#<CL-GPIO:PIN 17 :IN> #<CL-GPIO:PIN 18 :IN> #<CL-GPIO:PIN 19 :IN>
	#<CL-GPIO:PIN 2 :IN> #<CL-GPIO:PIN 20 :IN> #<CL-GPIO:PIN 21 :IN>
	#<CL-GPIO:PIN 22 :IN> #<CL-GPIO:PIN 23 :IN> #<CL-GPIO:PIN 24 :IN>
	#<CL-GPIO:PIN 25 :IN> #<CL-GPIO:PIN 26 :IN> #<CL-GPIO:PIN 3 :IN>
	#<CL-GPIO:PIN 4 :IN> #<CL-GPIO:PIN 5 :IN> #<CL-GPIO:PIN 6 :IN>
	#<CL-GPIO:PIN 7 :IN> #<CL-GPIO:PIN 8 :IN> #<CL-GPIO:PIN 9 :IN>)
	
You can then access the direction, edge, and active-low of each pin:

    (setf (gpio:direction 0) :out)
    (gpio:active-low 0)

If you try to read or set a PIN's value, its direction is automatically adjusted as necessary:

    (gpio:value 0)
    (setf (gpio:value 0) T)

On SBCL you can also wait for values:

    (progn (gpio:await-value 0)
           (format T "Whoah, 0's edge is ~a to ~:[0~;1~]" (edge 0) (value 0)))

Or even install handlers:

    (defun pin-value-handler (pin value)
      (format T "~& ~a changed value to ~a." pin value))
    (gpio:with-pin-handler (#'pin-value-handler 0 :falling)
      (format T "Waiting for a change on 0...")
      (loop (sleep 0.001)))

Naturally you'll have to refer to your particular board/system's specification to be able to tell which pins are supposed to be used for what. For the Raspberry Pi 2/3/4, it would be:

![rpi234-pin-mapping](https://raw.githubusercontent.com/Gadgetoid/Pinout.xyz/master/resources/raspberry-pi-pinout.png)

([Source](https://developer.microsoft.com/en-us/windows/iot/docs/pinmappingsrpi))
