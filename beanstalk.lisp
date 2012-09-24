(defpackage :beanstalk-async
  (:use #:cl)
  (:nicknames #:bas)
  (:export #:beanstalk-command
           #:parse-beanstalk-yaml))
(in-package :beanstalk-async)

(defparameter *headers* '((inserted id)
                          (buried id)
                          (expected_crlf)
                          (job_too_big)
                          (draining)
                          (using tube)
                          (deadline_soon)
                          (timed_out)
                          (reserved id bytes)
                          (deleted)
                          (not_found)
                          (released)
                          (touched)
                          (watching count)
                          (no_ignored)
                          (found id bytes)
                          (kicked count)
                          (kicked)
                          (ok bytes)
                          (paused))
  "Define each header and the variables it can return.")

(defparameter *errors* '(out_of_memory
                         internal_error
                         bad_format
                         unknown_command)
  "Defines the errors beanstalk can return (checked when processing headers).")

(define-condition out-of-memory (error) ())
(define-condition internal-error (error) ())
(define-condition bad-format (error) ())
(define-condition unknown-command (error) ())

(defun newlineize (string)
  "Replace instances of ~% (format's newline) with \r\n. Makes creating 
   beanstalkd strings a bit easier."
  (loop for pos = (search "~%" string)
        while pos do
    (setf (aref string pos) #\return
          (aref string (1+ pos)) #\newline))
  string)

(defun parse-header (str)
  "Parse the header of a beanstalk response."
  (let ((nl (search (vector #\return #\newline) str)))
    (when nl
      (let* ((header (subseq str 0 nl))
             (parts (split-sequence:split-sequence #\space header)))
        (let ((found (find-if (lambda (header)
                                (and (string= (car parts) (string (car header)))
                                     (= (length parts) (length header))))
                              *headers*))
              (result (list :header (car parts))))
          (if found
              (progn
                (loop for sym in (cdr found)
                      for val in (cdr parts) do
                  (setf result (append result (list (read-from-string (format nil ":~a" sym))
                                                    (read-from-string val)))))
                (values result (+ nl 2)))
              (let ((err (find-if (lambda (h)
                                      (string= (string h) header))
                                    *errors*)))
                (case err
                  (out_of_memory (error 'out-of-memory))
                  (internal_error (error 'internal-error))
                  (bad_format (error 'bad-format))
                  (unknown_command (error 'unknown-command))))))))))

(defun response-finished-p (str)
  "Check if a full response has been recieved from beanstalk. Since responses
   can come back in chunks, it makes sense to have a function that can tell if
   all chunks have een recieved."
  (multiple-value-bind (header offset) (parse-header str)
    (when header
      (if (getf header :bytes)
          (when (<= (getf header :bytes) (- (length str) offset))
            (values t
                    header
                    (subseq str offset (+ offset (getf header :bytes)))))
          (values t header)))))

(defun make-beanstalk-parser ()
  "Return a funciton that concatenates responses from various async operations
   until a full beanstalk response has been parsed, at which point the headers
   of the response and the response itself is returned."
  (let ((str ""))
    (lambda (data)
      (let ((string (if (stringp data)
                        data
                        (flexi-streams:octets-to-string data :external-format :utf8))))
        (setf str (concatenate 'string str string)))
      (let ((finishedp (multiple-value-list (response-finished-p str))))
        (when (car finishedp)
          (setf str ""))
        (apply #'values finishedp)))))

(defun parse-beanstalk-yaml (str)
  nil)

(defun beanstalk-command (command &key args finish-cb fail-cb data conn (read-timeout 5) (host "127.0.0.1") (port 11300))
  "Send a command to beanstalk asynchronously. If a connection is passed, it
   uses that instead of opening a new one. If this function is called without an
   event loop being initialized, it will start the event loop and block, so be
   sure to have your event loop started beforehand, unless this behavior is
   desired."
  ;; build the command
  (let* ((data (if (stringp data)
                   (flexi-streams:string-to-octets data :external-format :utf8)
                   data))
         (args (if data
                   (append args (list (length data)))
                   args))
         (cmd (with-output-to-string (s)
                (format s "~a" (string-downcase (string command)))
                (dolist (arg args)
                  (format s " ~a" arg))))
         (cmd (flexi-streams:string-to-octets
                (concatenate 'string cmd (newlineize "~%"))
                :external-format :utf8)))
    (when data
      ;; if we're sending data, provide the <bytes> variable and append the data
      (let* ((cmd-length (length cmd))
             (bytes (make-array (+ cmd-length (length data) 2) :element-type '(unsigned-byte 8))))
        (loop for i from 0
              for c across cmd do
          (setf (aref bytes i) c))
        (loop for i from cmd-length
              for d across data do
          (setf (aref bytes i) d))
        (setf (aref bytes (+ cmd-length 0 (length data))) 13
              (aref bytes (+ cmd-length 1 (length data))) 10)
        (setf cmd bytes)))
    ;; now make a beanstalk parser, which can recieve data in multiple chunks,
    ;; and run our command asynchronously.
    (let ((parser (make-beanstalk-parser)))
      (cl-async:tcp-async-send
         host port
         cmd
         (lambda (bev data)
           (multiple-value-bind (finishedp header response) (funcall parser data)
             (when (and finishedp finish-cb)
               ;; disable the bufferevent timeout so we can process the command
               (le::bufferevent-disable bev (logior le::+ev-read+ le::+ev-write+))
               ;; we got a full response, send it off to the finish cb
               (funcall finish-cb bev command header response))))
         fail-cb
         :read-timeout read-timeout
         :socket conn))))

