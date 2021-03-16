(uiop:define-package :utils/colors
    (:use #:cl
          #:uiop)
  (:export #:hsv2rgb))


(in-package #:utils/colors)

;;; Internal functions
(defun rgb->gray (r g b)
  "Convert an RGB color to grayscale. R, G, and B should be floating point
numbers in the range from 0.0 to 1.0."
  (let ((y (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))))
    (format nil "0x~x" (floor (* y 255)))))

(defun rgb2gray-usage ()
  (format t "Usage: rgb2gray RED GREEN BLUE~%")
  (format t "       RED, GREEN, and BLUE must be from 00 to FF.~%"))

(defun hsv->rgb (h s v)
  "Convert an HSL color to an RGB string. H is in the range zero to 360.
S and V are in the range from zero to 1.0."
  (let* ((i (floor (/ h 60.0)))
         (f (- (/ h 60.0) i))
         (p (* v (- 1.0 s)))
         (q (* v (- 1.0 (* f s))))
         (tt (* v (- 1.0 (* (- 1.0 f) s)))))
    (let ((rgb (case (mod i 6)
                 (0 (list v tt p))
                 (1 (list q v p))
                 (2 (list p v tt))
                 (3 (list p q v))
                 (4 (list tt p v))
                 (5 (list v p q)))))
      (loop for x in rgb collect (round (* x 255))))))

(defun hsv->rgb-str (h s v)
  "Convert the H, S, V color triplet into a hexadecial RGB color string.
H should be in the range from zero to 360. S and V are percentages and should
range from zero to 100."
  (format nil "~{~2,'0x~}" (hsv->rgb h s v)))

(defun in-range-p (value lo hi)
  "Determine if VALUE is greater than or equal to the LO value and less than
or equal to the HI value."
  (and (>= value lo) (<= value hi)))

(defun hsv2rgb-usage ()
  (format t "Usage: hsv2rgb HUE SATURATION VALUE~%"))

;;; Exported commands

(fare-utils:exporting-definitions
 (defun hsv2rgb (h-str s-str v-str)
   (let* ((h (parse-integer h-str))
          (s (/ (parse-integer s-str) 100.0))
          (v (/ (parse-integer v-str) 100.0)))
     (cond
       ((not (in-range-p h 0 360))
        (hsv2rgb-usage)
        (cl-scripting:fail! "Hue must be from 0 to 360, is ~a~%" h-str))
       ((not (in-range-p s 0.0 1.0))
        (hsv2rgb-usage)
        (cl-scripting:fail! "Saturation must be from 0 to 100 percent, is ~a~%" s-str))
       ((not (in-range-p v 0.0 1.0))
        (hsv2rgb-usage)
        (cl-scripting:fail! "Value must be from 0 to 100 percent, is ~a" v-str))
       (t (format! t "~a" (hsv->rgb-str h s v))
          (cl-scripting:success)))))

 (defun rgb2gray (r-str g-str b-str)
   (let ((r (/ (parse-integer r-str :radix 16) 255.0))
         (g (/ (parse-integer g-str :radix 16) 255.0))
         (b (/ (parse-integer b-str :radix 16) 255.0)))
     (cond
       ((not (and (in-range-p r 0.0 1.0)
                  (in-range-p g 0.0 1.0)
                  (in-range-p b 0.0 1.0)))
        (rgb2gray-usage)
        (cl-scripting:fail! "RGB components must be from 00 to FF"))
       (t (format! t "~a" (rgb->gray r g b))
          (cl-scripting:success))))))

(cl-scripting:register-commands :utils/colors)
