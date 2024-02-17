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

(defvar *custom-keys*
  '((sp1 . (holdtap (layer 1) space))
    (sp2 . (holdtap (layer 2) space))
    (sp3 . (holdtap (layer 3) space))
    (l1 . (layer 1))
    (l2 . (layer 2))
    (ctl . (holdtap ctrl esc))
    (hyper . (holdtap caps tab))
    ;; Home row mods
    (_a . (holdtap shft a))
    (_s . (holdtap alt s))
    (_d . (holdtap win d))
    (_f . (holdtap ctrl f))
    (_g . (holdtap caps g))
    (_h . (holdtap caps h))
    (_j . (holdtap ctrl j))
    (_k . (holdtap win k))
    (_l . (holdtap alt l))
    (_semi . (holdtap shft semi))))

;; Drawing of keyboard will be removed in processing of keymap.
;; See `strip-garbage' for details of ignored symbols.
(defvar *keymaps* '(
   ;; 0: Default layer
   (╭─────┬───┬───┬───┬───┬───╮       ╭────╮╭───┬───┬────┬────┬────┬────╮
    hyper │ q │ w │ e │ r │ t │      bspace │ y │ u │ i  │ o  │ p  │ esc
    ├─────┼───┼───┼───┼───┼───┤       ├────┤├───┼───┼────┼────┼────┼────┤
     shft │ a │ s │ d │ f │ g │       enter │ h │ j │ k  │ l   semi quot
    ├─────┼───┼───┼───┼───┼───┤       ╰────╯├───┼───┼────┼────┼────┼────┤
     alt  │ z │ x │ c │ v │ b │             │ n │ m comma dot slash shft
    ╰─────┴───┴───┴───┴───┼───┼─────╮ ╭─────┼───┼───┴────┴────┴────┴────╯
                           win l1      l2    ctl
                          ╰───┴─────╯ ╰─────┴───╯)
   ;; 1: Symbol layer
   (╭─────┬───┬───┬───┬───┬───╮       ╭────╮╭───┬───┬────┬────┬────┬────╮
    hyper │ q │ w │ e │ r │ t │      bspace │ y │ u │ i  │ o  │ p  │ esc
    ├─────┼───┼───┼───┼───┼───┤       ├────┤├───┼───┼────┼────┼────┼────┤
     shft │ a │ s │ d │ f │ g │       enter left down up right minus plus
    ├─────┼───┼───┼───┼───┼───┤       ╰────╯├───┼───┼────┼────┼────┼────┤
     alt  │ z │ x │ c │ v │ b │             │ n │ m comma dot quest shft
    ╰─────┴───┴───┴───┴───┼───┼─────╮ ╭─────┼───┼───┴────┴────┴────┴────╯
                           win l1      l3    ctl
                          ╰───┴─────╯ ╰─────┴───╯)
   ;; 2: Number layer
   (╭─────┬───┬───┬───┬───┬───╮       ╭────╮╭───┬───┬────┬────┬────┬────╮
    hyper │ 1 │ 2 │ 3 │ 4 │ 5 │      bspace │ 6 │ 7 │ 8  │ 9  │ 0  │ pipe
    ├─────┼───┼───┼───┼───┼───┤       ├────┤├───┼───┼────┼────┼────┼────┤
     shft  nil nil nil nil nil        enter home pgdn pgup end undsc bslash
    ├─────┼───┼───┼───┼───┼───┤       ╰────╯├───┼───┼────┼────┼────┼────┤
     alt   nil nil nil nil nil               nil nil  nil  nil  nil shft
    ╰─────┴───┴───┴───┴───┼───┼─────╮ ╭─────┼───┼───┴────┴────┴────┴────╯
                           win l3      l2    ctl
                          ╰───┴─────╯ ╰─────┴───╯)
   ;; 3: Function layer
   (╭─────┬───┬───┬───┬───┬───╮       ╭────╮╭───┬───┬────┬────┬────┬────╮
    hyper  f1  f2  f3  f4  f5 │      bspace  f6  f7 │ f8 │ f9  f10 │ nil
    ├─────┼───┼───┼───┼───┼───┤       ├────┤├───┼───┼────┼────┼────┼────┤
     shft  nil nil nil nil nil        enter  nil nil  nil  nil  nil  nil
    ├─────┼───┼───┼───┼───┼───┤       ╰────╯├───┼───┼────┼────┼────┼────┤
     alt   nil nil nil nil nil               nil nil  nil  nil  nil  shft
    ╰─────┴───┴───┴───┴───┼───┼─────╮ ╭─────┼───┼───┴────┴────┴────┴────╯
                           win l3      l2    ctl
                          ╰───┴─────╯ ╰─────┴───╯)

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

(defvar *i2c-port* 1)
(defvar *slave-address* #x08)

;; ------------------------------------------------------------------
;; Internal variables
;; ------------------------------------------------------------------


;; Keycodes are specific to the Arduino Keyboard library
;; Any printable ASCII character is represented by its ASCI
;; code. Other keys have special numbers.
;; https://www.arduino.cc/reference/en/language/functions/usb/keyboard/keyboardmodifiers/
(defvar *keycodes*
  (append *custom-keys*
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

    ;; Shifted keys
    ;; TODO: add keycodes directly
    (colon . (shft semi)) (undsc . (shft minus)) (dquot . (shft quot))
    (quest . (shft slash)) (pipe . (shft bslash)) (lt . (shft comma))
    (gt . (shft dot))

    )))


;; Holds the keys from `*keymap*' transformed to the physical row+col
;; layout defined by `*layout*'. Is initialized later.
(defvar *keymatrix* nil)


;; Holds the current state of the keyboard matrix.
(defvar *key-states*
  (make-array (+ 1 (max-in-array *layout*)) :element-type 'bit :initial-element 0))

(defvar *active-layers* (make-array 16 :element-type 'bit :initial-element 0))

;; Current layer
(defvar *layer* 0)

(defvar *cols* (length *col-pins*))
(defvar *rows* (length *row-pins*))
(defvar *matrix-length* (* *rows* *cols*))

;; ------------------------------------------------------------------
;; Global variables ends here
;; ------------------------------------------------------------------



;; ------------------------------------------------------------------
;; Utility functions
;; ------------------------------------------------------------------

(defun max-in-array (arr)
  "Find the largest value in array"
  (let ((max (aref arr 0)))
    (dotimes (pos (length arr))
      (let ((val (aref arr pos)))
        (when (> val max)
          (setf max val))))
    max))

(defun get-bit (byt b)
  "Get bit number `b' from `byt'"
  (if (plusp (logand (ash 1 b) byt)) 1 0))


;; ------------------------------------------------------------------
;; Validation functions
;; ------------------------------------------------------------------


(defun validate-keycodes ()
  "Make sure all entries in `*keycodes*' are valid, and that there
are no keycode collisions."
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


(defun validate-keymap ()
  "Make sure all `*keymaps*' and `*layout*' have the same geometry"
  (dolist (km *keymaps*)
    (unless (= (length (strip-garbage km)) (length *layout*))
      (format t "WARNING: Keymap and layout has different length!"))))


;; ------------------------------------------------------------------
;; Keymap cleanup functions
;; ------------------------------------------------------------------

(defun garbage? (elem)
  "Returns `t' if the element is garbage and should be
dropped from the keymap or layout."
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
(defun strip-garbage (keymap)
  "Remove comments from keymap"
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


(defun lookup-key (key)
  "Lookup the keycode or special key bound to `key'"
  (let ((string-key (princ-to-string key)))
    (if (= 1 (length string-key))
        (char-code (char string-key 0))
      (or (cdr (assoc key *keycodes*)) 0))))


(defun send-key (keycode pressed?)
  "Send a key press/release for the given keycode"
  (if (= pressed? 0)
      (keyboard/release keycode)
    (keyboard/press keycode)))


(defun activate-layer (layer)
  "Activate a layer"
  (format t "Activating layer ~a~%" layer)
  (setf *layer* layer))

(defun deactivate-layer (layer)
  "Deactivate a layer"
  (format t "Deactivating layer ~a~%" layer)
  (setf *layer* 0))

;; Handle a keypress event
(defun dispatch-key (pos pressed?)
  (let* ((key (aref *keymatrix* *layer* pos))
         (keycode (cond
                   ((symbolp key) (lookup-key key))
                   ((numberp key) (if (> key 9) key (lookup-key key)))
                   (t nil))))
    (cond
     ((numberp keycode) (send-key keycode pressed?))
     ;; Ignore any nil key
     ((null keycode) (format t "Ignoring key ~a~%" key))
     ((listp keycode)
      (format t "Handling special key ~a~%" keycode)
      (cond
       ((eq 'layer (car keycode))
        (let ((layer (cadr keycode)))
          (if (plusp pressed?)
              (activate-layer layer)
            (deactivate-layer layer))))
       ((eq 'holdtap (car keycode))
        (format t "HOLDTAP ~a~%" (cadr keycode)))
       ;; Else treat it as a list of normal keys
       (t (dolist (k keycode) (send-key (lookup-key k) pressed?))))))))



;; ------------------------------------------------------------------
;; Matrix scanning functions
;; ------------------------------------------------------------------

(defun process-slave-events ()
  "Poll key matrix from slave over I2C and process events."
  (let* ((key-pos *cols*)
         (bytes (with-i2c (str *i2c-port* *slave-address* *rows*)
                  (list (read-byte str)
                        (read-byte str)
                        (read-byte str)
                        (read-byte str)))))
    (dolist (byt bytes)
      (dotimes (b *cols*)
        (when (>= key-pos *matrix-length*)
          (return))
        (let ((key-state (aref *key-states* key-pos))
              (key-new-state (get-bit byt b)))
          (unless (eq key-state key-new-state)
            (setf (aref *key-states* key-pos) key-new-state)
            (dispatch-key key-pos key-new-state))
          (incf key-pos)))
      (setf key-pos (+ *cols* key-pos)))))



(defun process-events ()
  "Find which row+column is pressed, compare to previous state,
and dispatch key events."
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
            (dispatch-key key-pos key-new-state)))
        (incf key-pos)) ;; Increase position in flattened keymatrix
      ;; Disable row
      (digitalwrite row-pin t)
      ;; Skip other half of split keyboard
      (setf key-pos (+ *cols* key-pos)))
    (process-slave-events)))



;; ------------------------------------------------------------------
;; Init functions
;; ------------------------------------------------------------------

(defun initialize-keymatrix ()
  "Use `*layout*' to convert the visually laid out `*keymaps*' into
a matrix organized in rows and columns based on the eletrical layout"
  (setq *keymatrix* (make-array (list (length *keymaps*) (+ 1 (max-in-array *layout*))) :initial-element nil))
  (dotimes (layer (length *keymaps*))
    (let ((keymap-pos 0))
      (dolist (key (strip-garbage (nth layer *keymaps*)))
        (let ((matrix-pos (aref *layout* keymap-pos)))
          (setf (aref *keymatrix* layer matrix-pos) key))
        (incf keymap-pos)))))


(defun init ()
  "Set up IO pins, validate keymap and keycodes, initialize keyboard,
and calculate global variables."
  ;; Set up IO pins
  (dolist (col-pin *col-pins*)
    (pinmode col-pin :input-pullup))
  (dolist (row-pin *row-pins*)
    (pinmode row-pin :output))

  ;; Calculated values
  (setf *layer* 0)
  (setf *cols* (length *col-pins*))
  (setf *rows* (length *row-pins*))
  (setf *matrix-length* (* *rows* *cols*))

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
;; TODO: Make makefile or similar

(defun main ()
  "Main function. Run as a keyboard in an infinite loop."
  (init)
  (loop
    (process-events)))



)
