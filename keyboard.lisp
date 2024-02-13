(progn


(defvar *rows* 4)
(defvar *cols* 7) ;; TODO: find out how to handle slave


;; Using pinout for Elite-PI on ergotravel
;; Elite-PI: https://docs.keeb.io/assets/images/Elite-Pi_Pinout-Both-49caa4613444b03c72bc0c06bd622835.png
;; Ergotravel: https://github.com/jpconstantineau/ErgoTravel/blob/master/PCB/ErgoTravel_V1.03_Schematic.pdf
(defvar *row-pins*
  '(5 6 7 8))

(defvar *col-pins*
  '(28 27 26 22 20 23 21))

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

;; Drawing of keyboard will be removed in processing of keymap.
;; See `strip-garbage' for details of ignored symbols.
(defvar *keymap* '(
    ╭─────┬───┬───┬───┬───┬───╮       ╭────╮╭───┬───┬────┬────┬────┬────╮
    (tab  │ q │ w │ e │ r │ t │      bspace │ y │ u │ i  │ o  │ p  │ esc)
    ├─────┼───┼───┼───┼───┼───┤       ├────┤├───┼───┼────┼────┼────┬────╮
    (shft │ a │ s │ d │ f │ g │       enter │ h │ j │ k  │ l   semi quot)
    ├─────┼───┼───┼───┼───┼───┤       ╰────╯├───┼───┼────┼────┼────┼────┤
    (alt  │ z │ x │ c │ v │ b │             │ n │ m comma dot slash shft)
    ╰─────┴───┴───┴───┴───┼───┼─────╮ ╭─────┼───┼───┴────┴────┴────┴────╯
                          (win space   space ctrl)
                          ╰───┴─────╯ ╰─────┴───╯
))


;; This defines the translation from the physical placement of the keys to the position in the matrix.
;; It needs to be further translated from row and column to IO pin numbers
(defvar *layout*
  '(((1 0) (1 1) (0 2) (0 3) (0 4) (0 5)         (0 7) (0 8) (0 9) (0 10) (0 11) (1 12) (1 13))
    ((2 0) (2 1) (1 2) (1 3) (1 4) (1 5)         (1 7) (1 8) (1 9) (1 10) (1 11) (2 12) (2 13))
    ((3 0) (3 1) (2 2) (2 3) (2 4) (2 5)               (2 8) (2 9) (2 10) (2 11) (3 12) (3 13))
    (                              (3 4) (3 5)   (3 8) (3 9)                                  )))


;; Holds the current state of the keyboard matrix.
(defvar *key-states*
  (make-array (list *rows* *cols*) :element-type 'bit :initial-element 0))


;; Holds the keys from `*keymap*' transformed to the physical row+col
;; layout defined by `*layout*'. Is initialized later.
(defvar *keymatrix* nil)

;; --------------------------------------------------------------
;; Global variables ends here
;; --------------------------------------------------------------

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



;; Remove "comments" from keymap.
(defun strip-garbage (keymap)
  (if (null keymap)
      keymap
    (let* ((elem (car keymap))
           (string-elem (princ-to-string elem)))
      (cond
       ((listp elem)
        (cons (strip-garbage elem) (strip-garbage (cdr keymap))))
       ((or (search "│" string-elem)
            (search "─" string-elem)
            (search "╭" string-elem)
            (search "╮" string-elem)
            (search "╰" string-elem)
            (search "╯" string-elem)
            (search "┬" string-elem)
            (search "┴" string-elem)
            (search "├" string-elem)
            (search "┤" string-elem)
            (search "┼" string-elem))
        (strip-garbage (cdr keymap)))
       (t (cons elem (strip-garbage (cdr keymap))))))))



;; Use the `layout' to convert the visually laid out `keymap' into
;; a matrix organized in rows and columns based on the eletrical layout
(defun keymap->matrix (keymap layout)
  (let ((keymatrix (make-array (list *rows* 15))))
    (dotimes (row-number (length keymap))
      (let* ((keymap-row (nth row-number keymap))
             (layout-row (nth row-number layout))
             (row-length (length keymap-row))) ;; Assume equal length
        (dotimes (col-number row-length)
          (let* ((key (nth col-number keymap-row))
                 (matrix-pos (nth col-number layout-row))
                 (matrix-row (car matrix-pos))
                 (matrix-col (cadr matrix-pos)))
            (setf (aref keymatrix matrix-row matrix-col) key)))))
    keymatrix))


;; Make sure `keymap' and `layout' has the same geometry
(defun validate-keymap (keymap layout)
  (let ((keymap-rows (length keymap))
        (layout-rows (length layout)))
    (if (/= keymap-rows layout-rows)
        (format t "WARNING: Keymap and layout has different number of rows: ~a and ~a.~%"
                keymap-rows layout-rows)
      ;; Compare each row
      (dotimes (row-number keymap-rows)
        (let* ((keymap-row (nth row-number keymap))
               (layout-row (nth row-number layout))
               (keymap-row-length (length keymap-row))
               (layout-row-length (length layout-row)))
          (if (/= keymap-row-length layout-row-length)
              (format t "WARNING: Keymap and layout row ~a has different length: ~a and ~a.~%"
                      row-number keymap-row-length layout-row-length)))))))


;; TODO: Implement layers and layer keys
(defun lookup-key (key)
  (let ((string-key (princ-to-string key)))
    (if (= 1 (length string-key))
        (char-code (char string-key 0))
      (or (cdr (assoc key *keycodes*)) 0))))


;; Actually send the key
(defun send-key (row col pressed?)
  (let* ((key (aref *keymatrix* row col))
         (keycode (lookup-key key)))
    (if (= pressed? 0)
        (keyboard/release keycode)
      (keyboard/press keycode))))


;; Find which row+column is pressed,
;; compare to previous state, and send press/release events
(defun process-events ()
  (dotimes (col *cols*)
    (let ((col-pin (nth col *col-pins*)))
      ;; Enable column
      (digitalwrite col-pin t)
      ;; Read all keys in column
      (dotimes (row *rows*)
        (let* ((key-state (aref *key-states* row col))
               (row-pin (nth row *row-pins*))
               (key-new-state (if (digitalread row-pin) 1 0)))
    
          (unless (eq key-state key-new-state)
            (setf (aref *key-states* row col) key-new-state)
            (send-key row col key-new-state))))
      ;; Disable column
      (digitalwrite col-pin nil))))


(defun init ()
  ;; Set up IO pins
  (dolist (col-pin *col-pins*)
    (pinmode col-pin :output))
  (dolist (row-pin *row-pins*)
    (pinmode row-pin :input-pulldown))
  ;; Validate
  (validate-keycodes)
  (validate-keymap (strip-garbage *keymap*) *layout*)
  ;; Initialize keymatrix
  (setq *keymatrix* (keymap->matrix (strip-garbage *keymap*) *layout*))
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