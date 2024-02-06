;; -*- Lisp -*-

(defpackage :svg
  (:use :cl :alexandria))

(in-package :svg)

(defun split-path (path)
  (ppcre:split "\\s*(?=[a-zA-Z])" path))

(defun parse-command (command)
  (with-input-from-string (in command)
    (list (read-char in)
          (loop for x = (read in nil)
                for y = (read in nil)
                while x
                collect (cons x y)))))

(defun scale-commands (commands min-x max-x min-y max-y target-width y-offset)
  (declare (ignore max-y))
  (let ((factor (/ (- max-x min-x) target-width)))
    (mapcar (lambda (command)
              (cons (car command)
                    (mapcar (lambda (point)
                              (destructuring-bind (x . y) point
                                (cons (1+ (floor (- x min-x) factor))
                                      (+ 1 (floor (- y min-y) factor) y-offset))))
                            (cdr command))))
            commands)))

(defun path-to-commands (path-string width y-offset)
  (let (result
        x y
        min-x max-x
        min-y max-y)
    (flet ((track-extremes ()
             (cond
               (min-x
                (minf min-x x)
                (maxf max-x x)
                (minf min-y y)
                (maxf max-y y))
               (t
                (setf min-x x
                      max-x x
                      min-y y
                      max-y y)))))
      (dolist (command (split-path path-string))
        (destructuring-bind (command coordinates) (parse-command command)
          (ecase command
            (#\M
             (setf x (caar coordinates)
                   y (cdar coordinates))
             (track-extremes)
             (push `(:move (,x . ,y)) result))
            (#\L
             (push `(:line ,@(mapcar (lambda (coordinate)
                                       (destructuring-bind (ax . ay) coordinate
                                         (setf x ax
                                               y ay)
                                         (track-extremes)
                                         (cons ax ay)))
                                     coordinates))
                   result))
            ((#\l #\c)
             (push `(:line ,@(mapcar (lambda (coordinate)
                                       (destructuring-bind (dx . dy) coordinate
                                         (incf x dx)
                                         (incf y dy)
                                         (track-extremes)
                                         (cons x y)))
                                     coordinates))
                   result))
            (#\z)))))
    (format t "width: ~A height: ~A~%" (- max-x min-x) (- max-y min-y))
    (format t "min: ~A ~A max: ~A ~A~%" min-x min-y max-x max-y)
    (scale-commands (reverse result) min-x max-x min-y max-y width y-offset)))

(defun commands-to-regis (commands)
  (with-output-to-string (*standard-output*)
    (loop for (command . points) in commands
          do (princ (ecase command
                      (:move #\p)
                      (:line #\v)))
             (dolist (point points)
               (format t "[~A,~A]" (car point) (cdr point))))
    (terpri)))

(defun get-svg-path (svg-pathname)
  (let* ((doc (cxml:parse (alexandria:read-file-into-string svg-pathname) (stp:make-builder)))
         (node-set (xpath:with-namespaces (("" "http://www.w3.org/2000/svg"))
                     (xpath:evaluate "//path[not(ancestor::defs)]/@d" doc))))
    (assert (= (length (xpath:all-nodes node-set)) 1)
            () "input SVG needs to have exactly one path")
    (stp:value (xpath:first-node node-set))))

(defun svg-to-regis (svg-pathname &key (width 760) (y-offset 0))
  (commands-to-regis (path-to-commands (get-svg-path svg-pathname) width y-offset)))
