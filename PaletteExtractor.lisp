(load "~/.sbclrc")

(ql:quickload :opticl-core)
(ql:quickload :opticl)
(ql:quickload :bordeaux-threads)
(ql:quickload :cl-json)
(ql:quickload :iterate)

(use-package :iterate)

;; The difference tolerance is the maximum difference allowed between two pixels in rgb value for 
;; them to be grouped together
(defconstant +difference-tolerance+ 100)
(defconstant +number-of-threads+ 8)
(defconstant +image-directory+ "./images/")
(defconstant +palette-directory+ "./palettes/")
(defconstant +output-file+ "./data.json")

;; File names for images
(defparameter *image-files* nil)
(defparameter *threads* (list))
(defparameter *palette-dataset* (list))

;; We need a color-average class to represent a running average of a color extracted from an image.
;; This is also used to help the json encoder library extract values into json.
(defclass color-average ()
    ((r
        :initarg :r
        :initform (error "Must supply r"))
    (g
        :initarg :g
        :initform (error "Must supply g"))
    (b
        :initarg :b
        :initform (error "Must supply b"))
    (count
        :initarg :count
        :initform 1)))

;;Defining an output for the object.
(defmethod print-object ((object color-average) stream)
    (format stream "(r:~A g:~A b:~A | ~A)"
        (slot-value object 'r)
        (slot-value object 'g)
        (slot-value object 'b)
        (slot-value object 'count)))

;;Defining the encode-json method so the encoder can write the color palette results to a file.
(defmethod json:encode-json ((object color-average) &optional stream)
    (json:with-object (stream)
        (json:encode-object-member "r" (slot-value object 'r) stream)
        (json:encode-object-member "g" (slot-value object 'g) stream)
        (json:encode-object-member "b" (slot-value object 'b) stream)
        (json:encode-object-member "count" (slot-value object 'count) stream)))

(defun combine-average (p1 p2)
    (setf (slot-value p1 'r)
        (/ 
            (+ 
                (* (slot-value p2 'r) (slot-value p2 'count)) 
                (* (slot-value p1 'r) (slot-value p1 'count)))
            (+ (slot-value p1 'count) (slot-value p2 'count))))
    (setf (slot-value p1 'g)
        (/ 
            (+ 
                (* (slot-value p2 'g) (slot-value p2 'count)) 
                (* (slot-value p1 'g) (slot-value p1 'count)))
            (+ (slot-value p1 'count) (slot-value p2 'count))))
    (setf (slot-value p1 'b)
        (/ 
            (+ 
                (* (slot-value p2 'b) (slot-value p2 'count)) 
                (* (slot-value p1 'b) (slot-value p1 'count)))
            (+ (slot-value p1 'count) (slot-value p2 'count))))
    (setf (slot-value p1 'count) (+ (slot-value p1 'count) (slot-value p2 'count))))

(defun take-percent (palette percent)
    (let* ((result (list))
          (total (reduce #'+ (mapcar (lambda (i) (slot-value i 'count)) palette)))
          (threshold (* total percent))
          (sum 0))
        (loop for i in palette
              while (< sum threshold)
            do (push i result)
            (setf sum (+ sum (slot-value i 'count))))
        result))
    

;; Test data
;; (defparameter *test* (list
;;     (make-instance 'color-average :r 10.0 :g 9.0 :b 8.0 :count 10)
;;     (make-instance 'color-average :r 7.5 :g 6.3 :b 243.76 :count 4)
;;     (make-instance 'color-average :r 11.0 :g 11.0 :b 11.0 :count 17)
;;     (make-instance 'color-average :r 1.0 :g 7.0 :b 13.0 :count 1)
;;     (make-instance 'color-average :r 2.0 :g 8.0 :b 14.0 :count 2)
;;     (make-instance 'color-average :r 3.0 :g 9.0 :b 15.0 :count 3)
;;     (make-instance 'color-average :r 4.0 :g 10.0 :b 16.0 :count 4)
;;     (make-instance 'color-average :r 5.0 :g 11.0 :b 17.0 :count 5)
;;     (make-instance 'color-average :r 6.0 :g 12.0 :b 18.0 :count 6)))

;; Function that gets the absolute value of the difference between 2 values. This is used as a 
;; shortcut to map the difference between two lists.
(defun difference (a b) 
    ;; (format t "difference ~A ~A ~%" a b)
    (+
        (abs (- (slot-value a 'r) (slot-value b 'r)))
        (abs (- (slot-value a 'g) (slot-value b 'g)))
        (abs (- (slot-value a 'b) (slot-value b 'b)))))

;; Gets the value in list {l} that is closest to {value}. We need the closest value to determine 
;; which item we need to apply {value} to.
(defun closest (value l)
    ;; (format t "~A => ~A~%" value l)
    (if value
        (iter (for i in l)
            (finding i minimizing (difference value i)))))


;; Apply's {current-pixel} to the running average given by {average-color}. We need to keep
;; a running average for entries in order to keep the algorithm efficient.
(defun change-running-average (current-pixel average-color)
    (if (and current-pixel average-color)
        (combine-average average-color current-pixel)))

;; Processes a pixel by determining where it matches in palette-average and changing palette-average.
(defun process-pixel (current-pixel palette-average)
    (let* ((closest-pixel (closest current-pixel palette-average)))
        ;; (format t "~A -> ~A?~%" current-pixel palette-average)
        (cond 
            ((or (not closest-pixel)
                    (> (difference current-pixel closest-pixel) +difference-tolerance+))
             (push current-pixel palette-average))
            (t (let ((average-color (find closest-pixel palette-average)))
                (if average-color (change-running-average current-pixel average-color)))))
        palette-average))

;; Uses the color palette in {palette-average} to generate an image representation of the palette
(defun write-palette-results (img filename palette-average)
    (let ((i 0)
          (j 0))
        (opticl:with-image-bounds (height width) img 
            (loop for color-average in palette-average
                do (let ((r (round (slot-value color-average 'r)))
                        (g (round (slot-value color-average 'g)))
                        (b (round (slot-value color-average 'b))))
                        (loop for c below (slot-value color-average 'count)
                            do 
                            ;; (format t "i: ~A < ~A j: ~A < ~A = ~A~%" i height j width unsigned-color)
                            (if (>= j width) 
                                (block nil
                                    (setf j 0)
                                    (incf i)))
                            (if (< i height) (setf (opticl:pixel img i j) (values r g b)))
                            (incf j))))))
    (opticl:write-jpeg-file filename img))


(defun reduce-palette (palette new-size)
    (let ((temp-palette (copy-list palette)))
        (loop while (> (length temp-palette) new-size)
            do (let ((closest-difference nil)
                     (node1 nil)
                     (node2 nil))
                    (loop for i in temp-palette
                        do (loop for j in temp-palette
                            do (if (not (equal i j))
                                (if closest-difference
                                    (if (< (difference i j) closest-difference)
                                        (block nil
                                            (setf closest-difference (difference i j))
                                            (setf node1 i)
                                            (setf node2 j)))
                                    (block nil
                                        (setf closest-difference (difference i j))
                                        (setf node1 i)
                                        (setf node2 j))))))
                    (combine-average node1 node2)
                    (setf temp-palette (remove node2 temp-palette))
                    ;; (format t "~A - ~A~%~%" temp-palette (length temp-palette))
                    ))
        temp-palette))

;; Given a file name, the function will extract the color palette, and generate an image to the "palettes" folder
(defun extract-palette (file-name)
    (let ((palette-name (format nil "~A~A" +palette-directory+ (file-namestring file-name))))
        (format t "~A -> ~A~%" file-name palette-name)
        (ignore-errors 
            (time (let ((img (opticl:read-jpeg-file file-name))
            (palette-average (list)))
            (typecase img
                (opticl:8-bit-rgb-image
                    (locally
                        (declare (type opticl:8-bit-rgb-image img))
                        (opticl:with-image-bounds (height width) img 
                        (loop for i below height
                            do (loop for j below width 
                                do (multiple-value-bind (r g b) (opticl:pixel img i j)
                                    (declare (type (unsigned-byte 8) r g b))
                                    ;; (format t "R: ~D G: ~D B: ~D~%" r g b)
                                    (setf palette-average (process-pixel (make-instance 'color-average :r r :g g :b b) palette-average)))))
                        ;; (format t "~A: ~A~%~%" palette-average (length palette-average))
                        (let ((sorted-palette-average (sort palette-average #'> :key (lambda (pa) (slot-value pa 'count)))))
                            (write-palette-results img (format nil "~A.reduced.jpg" palette-name) sorted-palette-average)
                            ;; (format t "~A~%~%" sorted-palette-average)
                            (let* ((reduced-palette (reduce-palette (take-percent sorted-palette-average 0.9) 4))
                                   (sorted-reduced-palette (sort reduced-palette #'> :key (lambda (rp) (slot-value rp 'count)))))
                                ;; (format t "~A~%~%" sorted-reduced-palette)
                                (push sorted-reduced-palette *palette-dataset*)
                                (write-palette-results img palette-name sorted-reduced-palette))))))))))))

;; Pushes {thread} onto {thread-list} up to {+number-of-threads+}. If {thread-list} has {+number-of-threads+} elements
;; the function will pop the last element off of the list, waiting for the thread to finish, before pushing the thread.
(defun push-thread (thread thread-list)
    (if (< (length thread-list) +number-of-threads+)
        (push thread thread-list)
        (block nil
            ;; (format t "~A -> ~A~%" thread-list (mapcar #'sb-thread:thread-alive-p thread-list))
            (loop while (every (lambda (tr) (sb-thread:thread-alive-p tr)) thread-list)
                do (sleep 0.5))
            (let ((completed-thread (find-if (lambda (tr) (not (sb-thread:thread-alive-p tr))) thread-list)))
                (bt:join-thread completed-thread)
                (nsubstitute thread completed-thread thread-list)))))

;; Collecting a list of file names under the "images" directory
(uiop:collect-sub*directories +image-directory+
    (constantly t)
    (constantly t)
    (lambda (dir) 
        (loop for file-name in (uiop:directory-files dir)
            do (push file-name *image-files*))))

;; Creates the palette directory where the resulting color palette images will be written
(uiop:ensure-all-directories-exist (list +palette-directory+))

(setf *image-files* (delete-duplicates *image-files*))

;; Main loop of program
(time
    (block nil
        (loop for file-name in *image-files*
            do (setf *threads* 
                (push-thread 
                    (bt:make-thread (lambda () (extract-palette file-name))) 
                    *threads*)))
        (loop for thread in *threads*
            do (bt:join-thread thread))
        ;; (format t "~A~%" (json:encode-json *palette-dataset*))
        (let ((output-file (open +output-file+ :direction :output :if-exists :supersede)))
            (json:encode-json *palette-dataset* output-file)
            (close output-file))))

;; (defparameter *closest* (make-instance 'color-average :r 5 :g 9 :b 15))