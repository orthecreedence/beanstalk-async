(defpackage :beanstalk-async
  (:use #:cl #:cl-async-future)
  (:nicknames #:bas)
  (:export #:connect
           #:disconnect
           #:beanstalk-command
           #:parse-beanstalk-yaml))
(in-package :beanstalk-async)

(defparameter *headers* '((inserted id)
                          (buried)
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

(define-condition beanstalk-error (error) ())
(define-condition out-of-memory (beanstalk-error) ())
(define-condition internal-error (beanstalk-error) ())
(define-condition bad-format (beanstalk-error) ())
(define-condition unknown-command (beanstalk-error) ())

(defun convert-to-keyword (symbol)
  "Converts a symbol (ie TIMED_OUT) to :timed-out (note the _ to - replacement)"
  ;; no doubt, there is a better way to do this...
  (let ((str (string symbol)))
    (loop for i from 0
          for c across str do
      (when (char= c #\_)
        (setf (aref str i) #\-)))
    (intern str :keyword)))

(defun append-array (arr1 arr2)
  "Create an array, made up of arr1 followed by arr2."
  (let ((arr1-length (length arr1))
        (arr2-length (length arr2)))
    (let ((arr (make-array (+ arr1-length arr2-length)
                           :element-type (array-element-type arr1))))
      (replace arr arr1 :start1 0)
      (replace arr arr2 :start1 arr1-length)
      arr)))

(defun newlineize (string)
  "Replace instances of ~% (format's newline) with \r\n. Makes creating 
   beanstalkd strings a bit easier."
  (loop for pos = (search "~%" string)
        while pos do
    (setf (aref string pos) #\return
          (aref string (1+ pos)) #\newline))
  string)

(defun parse-beanstalk-yaml (str)
  "Turns beanstalk's YAML into a getf'able plist (or just a list, depending on
   the response)."
  (let* ((has-header-p (eq (search "---" str) 0))
         (yaml (if has-header-p
                   (subseq str (1+ (position #\newline str)))
                   str))
         (data nil))
    (let ((lines (split-sequence:split-sequence #\newline yaml)))
      (dolist (line lines)
        (unless (zerop (length line))
          (let ((split (search ": " line))
                (array-begin (search "- " line)))
            (cond
              (split
               (let ((key (read-from-string (concatenate 'string ":" (subseq line 0 split))))
                     (val (read-from-string (subseq line (1+ split)))))
                 (when (and key val)
                   (push val data)
                   (push key data))))
              ((eq array-begin 0)
               (push (subseq line 2) data)))))))
    data))

(defun parse-header (byte-array)
  "Parse the header of a beanstalk response."
  (let* ((str (babel:octets-to-string byte-array :encoding :utf-8))
         (nl (search (vector #\return #\newline) str)))
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

(defun response-finished-p (byte-array)
  "Check if a full response has been recieved from beanstalk. Since responses
   can come back in chunks, it makes sense to have a function that can tell if
   all chunks have een recieved."
  (multiple-value-bind (header offset) (parse-header byte-array)
    (when header
      (if (getf header :bytes)
          (when (<= (getf header :bytes) (- (length byte-array) offset))
            (values t
                    header
                    (subseq byte-array offset (+ offset (getf header :bytes)))))
          (values t header)))))

(defun make-beanstalk-parser ()
  "Return a funciton that concatenates responses from various async operations
   until a full beanstalk response has been parsed, at which point the headers
   of the response and the response itself is returned."
  (let ((data-arr nil))
    (lambda (data)
      (setf data-arr (if data-arr
                         (append-array data-arr data)
                         data))
      (let ((finishedp (multiple-value-list (response-finished-p data-arr))))
        (when (car finishedp)
          (setf data-arr nil))
        (apply #'values finishedp)))))

(defun connect (host port &key (read-timeout 5) event-cb)
  "Return a connection to a beanstalk server."
  (let ((future (make-future))
        (sock nil))
    (as:delay
      (lambda ()
        (finish future sock)))
    (set-event-handler future event-cb)
    (setf sock (as:tcp-connect host port
                 nil
                 (lambda (ev) (signal-event future ev))
                 :read-timeout read-timeout))))
>>>>>>> d4899467c149a8b8bc80268eba0ab8db3d504470

(defun disconnect (socket)
  "Close a beanstalk connection."
  (unless (as:socket-closed-p socket)
    (as:close-socket socket)
    t))

(defun beanstalk-command (command &key args format-cb write-cb data socket (read-timeout 5))
  "Send a command to beanstalk asynchronously. If a connection is passed, it
   uses that instead of opening a new one."
  ;; build the command
  (let* ((data (if (stringp data)
                   (babel:string-to-octets data :encoding :utf-8)
                   data))
         (args (if data
                   (append args (list (length data)))
                   args))
         (cmd (with-output-to-string (s)
                (format s "~a" (string-downcase (string command)))
                (dolist (arg args)
                  (format s " ~a" arg))))
         (cmd (babel:string-to-octets
                (concatenate 'string cmd (newlineize "~%"))
                :encoding :utf-8)))
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
    (let* ((parser (make-beanstalk-parser))
           (future (make-future))
           (event-cb (lambda (ev) (signal-event future ev)))
           (read-cb (lambda (socket data)
                      (multiple-value-bind (finishedp header response) (funcall parser data)
                        (when finishedp
                          ;; disable the bufferevent timeout so we can process the command
                          (as:disable-socket socket :read t :write t)
                          ;; we got a full response, either format it or send it
                          ;; straight to the finish-cb
                          (if format-cb
                              (funcall format-cb future command header response)
                              (finish future (list :header header :data response))))))))
      (as:set-socket-timeouts socket read-timeout nil)
      (as:write-socket-data socket cmd
                            :read-cb read-cb
                            :event-cb event-cb
                            :write-cb write-cb)
      future)))

