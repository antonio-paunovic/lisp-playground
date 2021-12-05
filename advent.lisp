;; SPC m g g - Go to definition.
;; SPC k W - wrap expression, insert current expression to the one below
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

(defun list-to-2d-array (list &optional (column-count 1))
  (let ((diagnostic-report (make-array column-count :fill-pointer 0 :adjustable t)))
    (loop :for row :in list do
      (vector-push-extend row diagnostic-report))
    diagnostic-report))

;; Test diagnostics.

(defvar *day3-test-input*
  '("00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000"
    "11001" "00010" "01010"))

;; Borrowed from https://stackoverflow.com/a/12327524
(defun array-slice (arr row)
  (make-array (array-dimension arr 1) 
              :displaced-to arr 
              :displaced-index-offset (* row (array-dimension arr 1))))

(defun filter-diagnostic-report (diagnostic-report column &key zero-or-one)
  "Filter out all entries in diagnostics which start with 0 | 1 character"
  (let ((row-count (array-dimension diagnostic-report 0))
        (filtered-diagnostic (make-array 1 :fill-pointer 0 :adjustable t)))
    (loop :for i :from 0 :below row-count
          :for row :across diagnostic-report do
            (when (not (equal zero-or-one (elt row column)))
              (vector-push-extend row filtered-diagnostic)))
    filtered-diagnostic))


(defun ones-and-zeros-count (diagnostic-report row-count column &key ignore ignore-col)
  (loop :for i :from 0 :below row-count
        :for row :across diagnostic-report
        if (not (and ignore
                     (equal ignore-col (elt row ignore-col)))) ; should not be '0' but current
          count (eql #\0 (elt row column)) into zeroes
          and count (eql #\1 (elt row column)) into ones
        finally (return (list zeroes ones))))

(defun calculate-gamma-rate (diagnostic-report row-count column-count)
  (let ((gamma-rate (make-array column-count :fill-pointer column-count :element-type 'character)))
    (loop :for j :from 0 :below column-count do
      (destructuring-bind (zeroes ones)
          (ones-and-zeros-count diagnostic-report row-count j)
        (if (>= ones zeroes)
            (setf (elt gamma-rate j) #\1)
            (setf (elt gamma-rate j) #\0))))
    (format t "gamma row ~d column ~d rate:~a ~%" row-count column-count gamma-rate)
    gamma-rate))

(defun calculate-epsilon-rate (diagnostic-report row-count column-count)
  (let ((epsilon-rate (make-array column-count :fill-pointer column-count :element-type 'character)))
    (loop :for j :from 0 :below column-count do
      (destructuring-bind (zeroes ones)
          (ones-and-zeros-count diagnostic-report row-count j)
        (if (> zeroes ones)
            (setf (elt epsilon-rate j) #\1)
            (setf (elt epsilon-rate j) #\0))))
    (format t "epsilon row ~d column ~d rate:~a ~%" row-count column-count epsilon-rate)
    epsilon-rate))

(defun calculate-oxygen-generator-rate (diagnostic-report row-count column-count)
  (let ((oxygen-generator-rate (make-array column-count :fill-pointer column-count :element-type 'character))
        (to-ignore nil)
        (filtered-diagnostic (filter-diagnostic-report diagnostic-report 0 :zero-or-one nil)))
    (loop :for j :from 0 :below column-count do
      (destructuring-bind (zeroes ones)
          (ones-and-zeros-count filtered-diagnostic
                                row-count j :ignore to-ignore :ignore-col j)
        (if (> zeroes ones)
            (progn (setf (elt oxygen-generator-rate j) #\1) (setf to-ignore #\1))
            (progn (setf (elt oxygen-generator-rate j) #\0) (setf to-ignore #\0)))
        (when (= (length filtered-diagnostic) 1)
          (return (vector-pop filtered-diagnostic)))
        (setf filtered-diagnostic (filter-diagnostic-report filtered-diagnostic j :zero-or-one to-ignore))
        ))
    (vector-pop filtered-diagnostic)))

(defun calculate-c02-scrubber-rate (diagnostic-report row-count column-count)
  (let ((oxygen-generator-rate (make-array column-count :fill-pointer column-count :element-type 'character))
        (to-ignore nil)
        (filtered-diagnostic (filter-diagnostic-report diagnostic-report 0 :zero-or-one nil)))
    (loop :for j :from 0 :below column-count do
      (destructuring-bind (zeroes ones)
          (ones-and-zeros-count filtered-diagnostic
                                row-count j :ignore to-ignore :ignore-col j)
        (if (>= ones zeroes )
            (progn (setf (elt oxygen-generator-rate j) #\1) (setf to-ignore #\1))
            (progn (setf (elt oxygen-generator-rate j) #\0) (setf to-ignore #\0)))
        (when (= (length filtered-diagnostic) 1)
          (return (vector-pop filtered-diagnostic)))
        (setf filtered-diagnostic (filter-diagnostic-report filtered-diagnostic j :zero-or-one to-ignore))
        ))
    ))




(defun day3 ()
  (let* ((diagnostic-report (list-to-2d-array (file-to-list "day3-input"))) ;))
         (column-count (length (elt diagnostic-report 0)))
         (row-count (length diagnostic-report)))
    (format t "Rank ~d ~d~%" column-count row-count)
    (format t "calculate-02-generator-rate ~d~%" (parse-integer (calculate-oxygen-generator-rate diagnostic-report row-count column-count) :radix 2))
    (format t "calculate-02-scrubber-rate ~d~%" (parse-integer (calculate-c02-scrubber-rate diagnostic-report row-count column-count) :radix 2))
    (* (parse-integer (calculate-gamma-rate diagnostic-report row-count column-count) :radix 2)
       (parse-integer (calculate-epsilon-rate diagnostic-report row-count column-count) :radix 2))))

(defun load-bingo-from-file (file-name)
  (with-open-file (file file-name)
    (cons (mapcar #'parse-integer (str:split #\, (read-line file nil nil)))
          (loop for line = (read-line file nil nil)
                while line
                if (> (length line) 0)
                  collect (list-to-2d-array 
                           (loop for i from 0
                                 while (and line (> (length line) 0))
                                 ;; do (format t "line: ~a~%" line)
                                 collect (map 'vector #'parse-integer (str:split #\Space line :omit-nulls t))
                                 do (setf line (read-line file nil nil)
                                          ) )
                           5) ) ) ))

(defun mark-point (board number &optional (mark nil))
  "Mark position on the board by the location of the `number` in that board."
  (let ((row-count (length board))
        (column-count (length (elt board 0))))
    (loop :for i :below row-count do
      (loop :for j :below column-count do
        (when (eq number (elt (elt board i ) j))
          (setf (elt (elt board i ) j) mark)
          (return ))))))

(defun bingo-victory-p (boards)
  "Check if there is a row or a column with all members marked."
  (loop :for board :in boards
        do (let ((row-marked-p (loop :for row :across board
                                     if (every #'null row)
                                       do (return t)))
                 (column-marked-p (loop :for column :below (length (elt board 0))
                                      if (every #'null (map 'vector #'(lambda (x) (elt x column)) board))
                                        do (return t))))
             (when (or row-marked-p column-marked-p)
               (return t))))
  )

(defun sum-unmarked-numbers (board)
  (loop :for row :across board
        sum (loop :for element :across row
                  if (numberp element)
                    sum element)))

(defun day4()
  "
  load-bingo-from-file
  list-to-2d-array
  for number in num-seq
    find point
    mark point
    check-victory-condition
"
  (let* ((bingo (load-bingo-from-file "day4-input"))
         (number-sequence (car bingo))
         (boards (cdr bingo)))
    (block bingo-end
      (loop :for number :in number-sequence do
        (loop :for board :in boards do
          (mark-point board number)
          (when (bingo-victory-p boards)
            (return-from bingo-end (list number board (sum-unmarked-numbers board)))))))
    ))
