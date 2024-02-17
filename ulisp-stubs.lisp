(defun digitalread (pin))
(defun digitalwrite (pin value))
(defun pinmode (pin mode))

(defun keyboard/begin ())
(defun keyboard/end ())
(defun keyboard/press (key))
(defun keyboard/release (key))
(defun keyboard/release-all ())

(defmacro with-serial (args &rest body))
(defmacro with-i2c (args &rest body))
(defmacro with-spi (args &rest body))
(defmacro with-sdcard (args &rest body))
(defun restart-i2c (&rest args))

(defun save-image (&rest args))
(defun load-image (&rest args))
