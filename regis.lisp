;; -*- Lisp -*-

(defpackage :regis
  (:use :cl :alexandria)
  (:export #:start-server
           #:with-regis))

(in-package :regis)

(defparameter *port* 11313)

(defparameter *serial-bitrate* 19200)
(defparameter *serial-chunk-size* 64)

(defvar *server-thread* nil)
(defvar *connection-handler* nil)

(define-condition exit-accept-loop ()
  ())

(defun read-upto-n-chars (stream max-len)
  (with-output-to-string (s)
    (loop for i below max-len
          for c = (read-char stream nil)
          while c
          do (write-char c s)
          finally (when (zerop i)
                    (return-from read-upto-n-chars nil)))))

(defun maybe-wait-for-xoff (stream)
  (when (equal (read-char-no-hang stream) #\DC3)
    (loop until (equal (read-char-no-hang stream) #\DC1)
          do (sleep .1))))

(defun paced-write-string (string &optional (stream *standard-output*))
  (with-input-from-string (input string)
    (loop for chunk = (read-upto-n-chars input *serial-chunk-size*)
          while chunk
          do (write-string chunk stream)
             (force-output stream)
             (sleep (* (length chunk) 10 (/ 1 *serial-bitrate*)))
             (maybe-wait-for-xoff stream))))

(defvar *connection-stream*)

(defun connection-handler (socket)
  (unwind-protect
       (loop (sleep 1))
    (format *trace-output* "~&; connection handler exiting~%")
    (usocket:socket-close socket)))

(defun try-signal-in-thread (thread condition)
  (handler-case
      (bt2:signal-in-thread thread condition)
    (error (e)
      (format *trace-output* "~&; signal-in-thread ~A failed: ~A~%" thread e))))

(defun accept-connections-loop ()
  "Thread function to loop and accept connections."
  (let (listen-socket)
    (handler-case
        (progn
          (setf listen-socket (usocket:socket-listen "0.0.0.0" *port* :reuseaddress t))
          (loop
            (let ((client-socket (usocket:socket-accept listen-socket)))
              (when client-socket
                (when *connection-handler*
                  (try-signal-in-thread *connection-handler* 'end-of-file))
                (setf *connection-handler* (bt2:make-thread (curry 'connection-handler client-socket)
                                                            :name "REGIS handler"
                                                            :initial-bindings (acons '*connection-stream* (usocket:socket-stream client-socket)
                                                                                     bt2:*default-special-bindings*)))))))
      (exit-accept-loop ()
        (format t "~&; exited accept loop~%"))
      (error (e)
        (format *error-output* "~&; error listening (~A), closing listening socket~%" e)))
    (ignore-errors (usocket:socket-close listen-socket))))

(defun stop-server ()
  (when *server-thread*
    (handler-case
        (bt2:signal-in-thread *server-thread* 'exit-accept-loop)
      (error (e)
        (format *error-output* "~&; could not stop server: ~A" e)))))

(defun start-server ()
  (stop-server)
  (setf *server-thread* (bt2:make-thread 'accept-connections-loop
                                         :name (format nil "REGIS listener on port ~A" *port*))))

(defun write-to-terminal (string)
  (bt2:interrupt-thread *connection-handler*
                        (lambda ()
                          (paced-write-string string *connection-stream*))))

(defmacro with-output-to-terminal (() &body body)
  `(with-output-to-string (*standard-output*)
     (prog1
         (progn ,@body)
       (write-to-terminal (get-output-stream-string *standard-output*)))))

(defmacro with-regis (() &body body)
  `(with-output-to-terminal ()
     (format t "~C[?4l" #\Escape)
     (format t "~C[H" #\Escape)
     (format t "~C[2J" #\Escape)
     (format t "~CPp" #\Escape)
     (write-string "
;S(I0 N0 A)
W(V I7 A0 S0 M1 N0 P1 M2)
T(I0 A0 D0 S1)
P[0,0]
")
     (prog1
         (progn ,@body)
       (format t "~C!" #\Control-Sequence-Introducer)
       (format t "~C[H" #\Escape)
       (force-output))))
