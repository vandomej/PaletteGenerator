(load "~/.sbclrc")

(ql:quickload "opticl-core")
(ql:quickload "opticl")
(ql:quickload "lparallel")
(ql:quickload "bordeaux-threads")

;; The difference tolerance is the maximum difference allowed between two pixels in rgb value for 
;; them to be grouped together
(defconstant +difference-tolerance+ 100)
(defconstant +number-of-threads+ 8)
(defconstant +image-directory+ "./images/")
(defconstant +palette-directory+ "./palettes")

;; File names for images
(defparameter *image-files* nil)
(defparameter *threads* (list))

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

;; Apply's {current-pixel} to the running average given by {average-pixel}. We need to keep
;; a running average for entries in order to keep the algorithm efficient.
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
        ;; (format t "~A < ~A?~%" (reduce #'+ (mapcar #'difference closestcurrent-pixel closest-pixel)) +difference-tolerance+)
        (cond 
            ((or 
                (not closest-pixel)
                (> (reduce #'+ (mapcar #'difference current-pixel closest-pixel)) +difference-tolerance+))
                (push (list current-pixel 1) palette-average))
            (t 
                (let ((average-pixel (find closest-pixel palette-average :key #'first)))
                    (if average-pixel (setf average-pixel (change-running-average current-pixel average-pixel))))))
        palette-average))

;; Uses the color palette in {palette-average} to generate an image representation of the palette
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

;; Given a file name, the function will extract the color palette, and generate an image to the "palettes" folder
(defun extract-palette (file-name)
    (let ((palette-name (format nil "~A~A" +palette-directory+ (file-namestring file-name))))
        (format t "~A -> ~A~%" file-name palette-name)
        (ignore-errors
            (time
                (let ((img (opticl:read-jpeg-file file-name))
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
                                ;; (format t "~A: ~A~%~%" palette-average (length palette-average))
                                (write-palette-results img palette-name (sort palette-average #'> :key #'second)))))))))))

;; Pushes {thread} onto {thread-list} up to {+number-of-threads+}. If {thread-list} has {+number-of-threads+} elements
;; the function will pop the last element off of the list, waiting for the thread to finish, before pushing the thread.
(defun push-thread (thread thread-list)
    (if (< (length thread-list) +number-of-threads+)
        (append thread-list (list thread))
        (block nil
            (bordeaux-threads:join-thread (first thread-list))
            (pop thread-list)
            (append thread-list (list thread)))))

;; Collecting a list of file names under the "images" directory
(uiop:collect-sub*directories +image-directory+
    (constantly t)
    (constantly t)
    (lambda (dir) 
        (loop for file-name in (uiop:directory-files dir)
            do (push file-name *image-files*))))

;; Creates the palette directory where the resulting color palette images will be written
(uiop:ensure-all-directories-exist (list +palette-directory+))

;; Main loop of program
(time
    (loop for file-name in *image-files*
        do (setf *threads* 
            (push-thread 
                (bordeaux-threads:make-thread 
                    (lambda () (extract-palette file-name))) 
                *threads*))))
