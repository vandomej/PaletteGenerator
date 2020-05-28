(load "~/.sbclrc")

(ql:quickload "opticl-core")
(ql:quickload "opticl")
(ql:quickload "lparallel")

;; (setf lparallel:*kernel* (lparallel:make-kernel 8))

(defparameter *difference-tolerance* 200)

;; Defining currying for later use
(declaim (ftype (function (function &rest t) function) curry)
    (inline curry))
(defun curry (function &rest args)
    (lambda (&rest more-args)
      (apply function (append args more-args))))

;; Function that gets the absolute value of the difference between 2 values. This is used as a 
;; shortcut to map the difference between two lists.
(defun difference (a b) 
    ;; (format t "difference ~A ~A ~%" a b)
    (abs (- a b)))

;; Gets the value in list {l} that is closest to {value}. We need the closest value to determine 
;; which item we need to apply {value} to.
(defun closest (value l)
    ;; (format t "~A => ~A~%" value l)
    (if value
        (let ((closest-val) (closest-difference))
            (loop for i in l
                do 
                    ;; (format t "i: ~A~%" i)
                    (let ((temp-difference (reduce #'+ (mapcar #'difference value (first i)))))
                        (cond ((or (not closest-val)
                                (< temp-difference closest-difference)) 
                                    (setf closest-val (first i))
                                    (setf closest-difference temp-difference)))))
            closest-val)))

;; Apply's {current-pixel} to the running average given by {closest-pixel} in {l}. We need to keep
;; a running average for entries in {l} in order to keep the algorithm efficient.
(defun change-running-average (current-pixel average-pixel)
    (if (and current-pixel average-pixel)
        (block nil
            (setf (first average-pixel) 
                (mapcar #'(lambda (a)
                    (/ a (+ 1 (second average-pixel)))) 
                    (mapcar #'+ 
                        current-pixel
                        (mapcar (curry #'* (second average-pixel)) (first average-pixel)))))
            (incf (second average-pixel))
            average-pixel)))

;; Processes a pixel by determining where it matches in palette-average and changing palette-average.
(defun process-pixel (current-pixel palette-average)
    (let* ((closest-pixel (closest current-pixel palette-average)))
        ;; (format t "~A < ~A?~%" (reduce #'+ (mapcar #'difference current-pixel closest-pixel)) *difference-tolerance*)
        (cond 
            ((or 
                (not closest-pixel)
                (> (reduce #'+ (mapcar #'difference current-pixel closest-pixel)) *difference-tolerance*))
                (push (list current-pixel 1) palette-average))
            (t 
                (let ((average-pixel (find closest-pixel palette-average :key #'first)))
                    (if average-pixel (setf average-pixel (change-running-average current-pixel average-pixel))))))
        palette-average))

;; Sorts 
(defun write-palette-results (img filename palette-average)
    (let ((i 0)
          (j 0))
        (opticl:with-image-bounds (height width) img 
            (loop for (color count) in palette-average
                do (let ((unsigned-color (mapcar #'round color)))
                    (loop for c below count
                        do 
                            ;; (format t "i: ~A < ~A j: ~A < ~A = ~A~%" i height j width unsigned-color)
                            (if (>= j width) 
                                (block nil
                                    (setf j 0)
                                    (incf i)))
                            (if (< i height) (setf (opticl:pixel img i j) (values-list unsigned-color)))
                            (incf j))))))
    (opticl:write-jpeg-file filename img))

;; Main loop, loading an image and looping through it pixel by pixel.
(time
    (let ((img (opticl:read-jpeg-file "./Images/image.jpg"))
        (palette-average (list)))
        (typecase img
            (opticl:8-bit-rgb-image
                (locally
                    (declare (type opticl:8-bit-rgb-image img))
                    (opticl:with-image-bounds (height width) img 
                    (loop for i below height
                        ;; do (dotimes (j width t)
                        do (loop for j below width 
                            do (multiple-value-bind (r g b) (opticl:pixel img i j)
                                    (declare (type (unsigned-byte 8) r g b))
                                    ;; (format t "R: ~D G: ~D B: ~D~%" r g b)
                                    (setf palette-average (process-pixel (list r g b) palette-average)))))
                    (format t "~A: ~A~%~%" palette-average (length palette-average))
                    (write-palette-results img "./Images/inv-image.jpeg" (sort palette-average #'> :key #'second))))))))

;; Writing the img object back into a file
;;   (opticl:write-jpeg-file "./inv-image.jpeg" img)

;; (lparallel:end-kernel :wait t)