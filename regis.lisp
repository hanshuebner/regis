;; -*- Lisp -*-

(defpackage :regis
  (:use :cl :alexandria)
  (:export #:start-server
           #:with-regis))

(in-package :regis)

(defparameter *port* 11313)

(defvar *server-thread* nil)
(defvar *terminal-socket* nil)

(define-condition exit-accept-loop ()
  ())

(defun accept-connections-loop ()
  "Thread function to loop and accept connections."
  (let (listen-socket)
    (handler-case
        (progn
          (setf listen-socket (usocket:socket-listen "0.0.0.0" *port* :reuseaddress t))
          (loop
            (let ((client-socket (usocket:socket-accept listen-socket)))
              (when client-socket
                (when *terminal-socket*
                  (usocket:socket-close *terminal-socket*))
                (setf *terminal-socket* client-socket)))))
      (exit-accept-loop ()
        (format t "~&; exited accept loop~%"))
      (error (e)
        (format *error-output* "~&; error listening (~A), closing listening socket~%" e)))
    (ignore-errors (usocket:socket-close listen-socket))))

(defun stop-server ()
  (when *server-thread*
    (handler-case
        (bordeaux-threads-2:signal-in-thread *server-thread* 'exit-accept-loop)
      (error (e)
        (format *error-output* "~&; could not stop server: ~A" e)))))

(defun start-server ()
  (stop-server)
  (setf *server-thread* (bordeaux-threads-2:make-thread 'accept-connections-loop
                                                        :name (format nil "REGIS listener on port ~A" *port*))))



(defmacro with-output-to-terminal (() &body body)
  `(let ((*standard-output* (usocket:socket-stream *terminal-socket*)))
     (prog1
         (progn ,@body)
       (force-output))))

(defmacro with-regis ((&optional (stream *standard-output*)) &body body)
  `(with-output-to-terminal ()
     (format t "~C[?4l" #\Escape)
     (format t "~Cc" #\Control-Sequence-Introducer)
     (force-output)
     (sleep 1)
     (format t "~CPp" #\Escape)
     (write-string "
s(E I0)
W(I7 P1 M2 A0 N0 S0)
T(A0 I0 D0 S1)
")
     (prog1
         (progn ,@body)
       (format t "~C!" #\Control-Sequence-Introducer)
       (format t "~C[H" #\Escape))))
