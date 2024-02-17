(progn

;; ------------------------------------------------------------------
;; User variables
;; ------------------------------------------------------------------

;; Using pinout for Elite-PI on ergotravel
;; Elite-PI: https://docs.keeb.io/assets/images/Elite-Pi_Pinout-Both-49caa4613444b03c72bc0c06bd622835.png
;; Ergotravel: https://github.com/jpconstantineau/ErgoTravel/blob/master/PCB/ErgoTravel_V1.03_Schematic.pdf
(defvar *row-pins*
  '(5 6 7 8))

(defvar *col-pins*
  '(28 27 26 22 20 23 21))

;; Drawing of keyboard will be removed in processing of keymap.
;; See `strip-garbage' for details of ignored symbols.
(defvar *keymap* '(
    ╭─────┬───┬───┬───┬───┬───╮       ╭────╮╭───┬───┬────┬────┬────┬────╮
     tab  │ q │ w │ e │ r │ t │      bspace │ y │ u │ i  │ o  │ p  │ esc
    ├─────┼───┼───┼───┼───┼───┤       ├────┤├───┼───┼────┼────┼────┼────┤
     shft │ a │ s │ d │ f │ g │       enter │ h │ j │ k  │ l   semi quot
    ├─────┼───┼───┼───┼───┼───┤       ╰────╯├───┼───┼────┼────┼────┼────┤
     alt  │ z │ x │ c │ v │ b │             │ n │ m comma dot slash shft
    ╰─────┴───┴───┴───┴───┼───┼─────╮ ╭─────┼───┼───┴────┴────┴────┴────╯
                           win space   space ctrl
                          ╰───┴─────╯ ╰─────┴───╯
))


;; This defines the translation from the physical placement of the
;; keys to the position in the flattened keymatrix. To calculate the
;; indexes listed here take the row and column number from the actual
;; electrical layout of the keymatrix. Indexes here can be calculated
;; as `row-number * number-of-columns + col-number`.
(defvar *layout* #(
      14   15   2   3   4   5            7    8   9   10   11   26   27
      28   29  16  17  18  19           21   22  23   24   25   40   41
      42   43  30  31  32  33                36  37   38   39   54   55
                           46   47      50   51 ))


;; ------------------------------------------------------------------
;; Internal variables
;; ------------------------------------------------------------------


;; Keycodes are specific to the Arduino Keyboard library
;; Any printable ASCII character is represented by its ASCI
;; code. Other keys have special numbers.
;; https://www.arduino.cc/reference/en/language/functions/usb/keyboard/keyboardmodifiers/
(defvar *keycodes*
  '(;; Modifiers
    (ctrl . #x80) (shft . #x81) (alt . #x82) (win . #x83)
    (rctrl . #x84) (rshft . #x85) (ralt . #x86) (rwin . #x87)

    ;; Symbols
    (lbkt . #x5B) (rbkt . #x5D) (dot . #x2E) (comma . #x2C)
    (semi . #x3B) (quot . #x27) (minus . #x2D) (grave . #x60)
    (slash . #x2F) (bslash . #x5C) (equal . #x3D)

    ;; Arrow keys
    (left . #xD8) (right . #xD7) (up . #xDA) (down . #xD9)

    ;; Navigation keys
    (pgup . #xD3) (pgdn . #xD6) (home . #xD2) (end . #xD5)

    ;; Other keys
    (space . #x20) (bspace . #xB2) (enter . #xB0) (caps . #xC1)
    (esc . #xB1) (delete . #xD4) (insert . #xD1) (tab . #xB3)

    ;; F-keys
    (f1 . #xC2) (f2 . #xC3) (f3 . #xC4) (f4  . #xC5) (f5  . #xC6) (f6  . #xC7)
    (f7 . #xC8) (f8 . #xC9) (f9 . #xCA) (f10 . #xCB) (f11 . #xCC) (f12 . #xCD)

    ))


;; Holds the keys from `*keymap*' transformed to the physical row+col
;; layout defined by `*layout*'. Is initialized later.
(defvar *keymatrix* nil)


;; Holds the current state of the keyboard matrix.
(defvar *key-states*
  (make-array (+ 1 (max-in-array *layout*)) :element-type 'bit :initial-element 0))

;; ------------------------------------------------------------------
;; Global variables ends here
;; ------------------------------------------------------------------



;; ------------------------------------------------------------------
;; Utility functions
;; ------------------------------------------------------------------

;; Return the biggest number in array
(defun max-in-array (arr)
  (let ((max (aref arr 0)))
    (dotimes (pos (length arr))
      (let ((val (aref arr pos)))
        (when (> val max)
          (setf max val))))
    max))

(defun get-bit (byt b)
  (if (plusp (logand (ash 1 b) byt)) 1 0))


;; ------------------------------------------------------------------
;; Validation functions
;; ------------------------------------------------------------------


;; Make sure all entries in `*keycodes*' are valid,
;; and that there are no keycode collisions.
(defun validate-keycodes ()
  (dolist (key-a *keycodes*)
    (dolist (key-b *keycodes*)
      ;; Only compare key-a to keys before it
      ;; to avoid comparing the same pair twice
      (when (equal key-a key-b)
        (return))
      ;; TODO: Check if pair
      (let ((symbol-a (car key-a))
            (symbol-b (car key-b))
            (keycode-a (cdr key-a))
            (keycode-b (cdr key-b)))
        (when (equal keycode-a keycode-b)
          (format t "WARNING: Symbols ~a and ~a both have keycode ~a.~%"
                  symbol-a symbol-b keycode-a))))))


;; Make sure `*keymap*' and `*layout*' has the same geometry
(defun validate-keymap ()
  (unless (= (length (strip-garbage *keymap*)) (length *layout*))
    (format t "WARNING: Keymap and layout has different length!")))


;; ------------------------------------------------------------------
;; Keymap cleanup functions
;; ------------------------------------------------------------------

;; Returns `t' if the element is "garbage" and should be
;; dropped from the keymap or layout.
(defun garbage? (elem)
  (or (eq '│ elem)
      (let ((string-elem (princ-to-string elem)))
        (or (search "─" string-elem)
            (search "╰" string-elem)
            (search "╭" string-elem)
            (search "┼" string-elem)
            (search "╮" string-elem)
            (search "╯" string-elem)
            (search "┬" string-elem)
            (search "┴" string-elem)
            (search "├" string-elem)
            (search "┤" string-elem)))))


;; TODO: Figure out why the long symbols are split
;; Remove "comments" from keymap.
(defun strip-garbage (keymap)
  (if (null keymap)
      keymap
    (let ((elem (car keymap)))
      (if (garbage? elem)
        (progn
          (format t "Stripping symbol from keymap: ~a~%" elem)
          (strip-garbage (cdr keymap)))
        (cons elem (strip-garbage (cdr keymap)))))))



;; ------------------------------------------------------------------
;; Key dispatch and lookup functions
;; ------------------------------------------------------------------


;; TODO: Implement layers and layer keys
(defun lookup-key (key)
  (let ((string-key (princ-to-string key)))
    (if (= 1 (length string-key))
        (char-code (char string-key 0))
      (or (cdr (assoc key *keycodes*)) 0))))


;; Actually send the key
(defun send-key (pos pressed?)
  (let* ((key (aref *keymatrix* pos))
         (keycode (lookup-key key)))
    (if (= pressed? 0)
        (keyboard/release keycode)
      (keyboard/press keycode))))





;; ------------------------------------------------------------------
;; Matrix scanning functions
;; ------------------------------------------------------------------

(defun process-slave-events ()
  (with-i2c (str 1 #x08 4)
    (let ((key-pos 7))
      (dotimes (x 4)
        (let ((byt (read-byte str)))
          (dotimes (b 7)
            (when (>= key-pos 56)
              (return))
            (let ((key-state (aref *key-states* key-pos))
                  (key-new-state (get-bit byt b)))
              (unless (eq key-state key-new-state)
                (setf (aref *key-states* key-pos) key-new-state)
                (send-key key-pos key-new-state))
              (incf key-pos)))
          (setf key-pos (+ 7 key-pos)))))))



;; TODO: Try inverting the logic, i.e. use pullups on the columns
;; and pulling the rows low.
;; Find which row+column is pressed,
;; compare to previous state, and send press/release events
(defun process-events ()
  (let ((key-pos 0)) ;; This is the position in the flattened keymatrix
    (dolist (row-pin *row-pins*)
      ;; Enable row
      (digitalwrite row-pin nil)
      ;; Read all keys in row
      (dolist (col-pin *col-pins*)
        (let* ((key-state (aref *key-states* key-pos))
               (key-new-state (if (digitalread col-pin) 0 1)))

          (unless (eq key-state key-new-state)
            (setf (aref *key-states* key-pos) key-new-state)
            (send-key key-pos key-new-state)))
        (incf key-pos)) ;; Increase position in flattened keymatrix
      ;; Disable row
      (digitalwrite row-pin t)
      ;; Skip other half of split keyboard
      (setf key-pos (+ 7 key-pos)))
    (process-slave-events)))



;; ------------------------------------------------------------------
;; Init functions
;; ------------------------------------------------------------------

;; Use the `layout' to convert the visually laid out `keymap' into
;; a matrix organized in rows and columns based on the eletrical layout
(defun initialize-keymatrix ()
  (setq *keymatrix* (make-array (+ 1 (max-in-array *layout*)) :initial-element 0))
    (let ((keymap-pos 0))
    (dolist (key (strip-garbage *keymap*))
      (let ((matrix-pos (aref *layout* keymap-pos)))
        (setf (aref *keymatrix* matrix-pos) key))
      (incf keymap-pos))))

(defun init ()
  ;; Set up IO pins
  (dolist (col-pin *col-pins*)
    (pinmode col-pin :input-pullup))
  (dolist (row-pin *row-pins*)
    (pinmode row-pin :output))
  ;; Validate
  (validate-keycodes)
  (validate-keymap)
  ;; Initialize keymatrix
  (initialize-keymatrix)
  ;; Initialize USB HID keyboard
  (keyboard/begin))



;; TODO: Add ulisp as submodule or directly
;; TODO: Make ulisp fork
;; TODO: Add dockerfile
;; TODO: Write short instructions for setup/installation
;; TODO: Write docstrings

(defun main ()
  (init)
  (loop
    (process-events)))



)
