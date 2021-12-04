(defpackage :advent
  (:use :cl))

(in-package :advent)

(ql:quickload "str")
;; (ql:quickload :coalton)

(defun file-to-list (file-name)
  (with-open-file (file file-name)
    (loop for i from 0
          for line = (read-line file nil nil)
          while line
          collect line
          do (format t "~d: ~a~%" i line))))

(defun day1 ()
  (let* ((nums (mapcar #'parse-integer (file-to-list "day1-input")))
         (sums (loop for i in nums
                     for j in (rest nums)
                     for k in (rest (rest nums))
                     collect (+ i j k))))
    (count "increase"
           (mapcar #'(lambda (x y) (if (> y x) "increase" "decrease"))
                   sums (rest sums))
           :test #'equal)))

;; (coalton:coalton-toplevel
;;   (define (slope xs)
;;     (match (tail xs)
;;       ((Some ys) (sum (zipwith (fn (x y) (if (> y x) 1 0)) xs ys)))
;;       (None 0))))


(defstruct pos x y aim)

(defun day2 ()
  (let ((directions
          (mapcar #'(lambda (action)
                      (destructuring-bind (direction . step) (str:split #\Space action)
                        (cons direction (parse-integer (first step)))))
                   ;; (list "forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2")
                  (file-to-list "day2-input"))))
    (reduce #'(lambda (current-pos action)
                (destructuring-bind (direction . step) action
                  (cond
                    ((equalp direction "forward")
                     (setf (pos-x current-pos) (+ (pos-x current-pos) step))
                     (setf (pos-y current-pos) (+ (pos-y current-pos) (* (pos-aim current-pos) step))))
                    ((equalp direction "down")
                     (setf (pos-aim current-pos) (+ (pos-aim current-pos) step)))
                    ((equalp direction "up")
                     (setf (pos-aim current-pos) (- (pos-aim current-pos) step)))
                    (t current-pos))
                  current-pos))
            directions
            :initial-value (make-pos :x 0 :y 0 :aim 0))))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

;; Test diagnostics.
;; ("00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000"
;; "11001" "00010" "01010")
(defun day3 ()
  (let* ((diagnostic-report (list-to-2d-array (file-to-list "day3-input")))
         (column-count (array-dimension diagnostic-report 1))
         (row-count (array-dimension diagnostic-report 0))
         (gamma-rate (make-array column-count :fill-pointer column-count :element-type 'character))
         (epsilon-rate (make-array column-count :fill-pointer column-count :element-type 'character)))
    (format t "Rank ~d ~d~%" column-count row-count)
    (loop :for j :from 0 :below column-count do
      (let ((big (loop :for i :from 0 :below row-count
                       count (eql #\0 (aref diagnostic-report i j)) into zeroes
                       count (eql #\1 (aref diagnostic-report i j)) into ones
                       finally (return (progn (format t "~d ~d ~%" zeroes ones)
                                              (if (> zeroes ones) 0 1))))))
        (if (= 1 big)
            (progn (setf (elt gamma-rate j) #\1)
                   (setf (elt epsilon-rate j) #\0))
            (progn (setf (elt gamma-rate j) #\0)
                   (setf (elt epsilon-rate j) #\1)))))
    (* (parse-integer gamma-rate :radix 2) (parse-integer epsilon-rate :radix 2))))
