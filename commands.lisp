(in-package :beanstalk-async)

(defparameter *command-alias*
  '((del . delete))
  "Defines commands that will be renamed (from . to) in the defcommand macro.")

(defun find-command-alias (fn-name)
  "Lookup command alias, retuning the original function symbol if not found."
  (let ((alias (find-if (lambda (a) (eq (car a) fn-name)) *command-alias*)))
    (if alias
        (cdr alias)
        fn-name)))

(defmacro defcommand (fn-name args &key sends-data format-cb possible-errors)
  "Makes creating new commands stupid easy."
  `(progn
     (defun ,fn-name
            ,(append args
                     (when sends-data '(data))
                     '(&key finish-cb event-cb write-cb socket (read-timeout 5) (host "127.0.0.1") (port 11300)))
       (let ((args (list ,@args)))
         (beanstalk-command (string-downcase (string (find-command-alias ',fn-name)))
                            :args args
                            ,@(when sends-data '(:data data))
                            ,@(when format-cb `(:format-cb ,format-cb))
                            ,@(when possible-errors `(:possible-errors ,possible-errors))
                            :finish-cb finish-cb
                            :event-cb event-cb
                            :write-cb write-cb
                            :socket socket
                            :read-timeout read-timeout
                            :host host
                            :port port)))
     (export ',fn-name :beanstalk-async)))

;; -----------------------------------------------------------------------------
;; define commands used for formatting data returned from beanstalk
;; TODO: code duplication runs rampant. fix.
;; -----------------------------------------------------------------------------
(defun format-with-status-id-data (finish-cb socket command header response)
  "Grab the ID from the header and the data (if any) and send them into the
   data arg of the finish-cb."
  (declare (ignore command))
  (let ((id (getf header :id))
        (status (convert-to-keyword (getf header :header))))
    (funcall finish-cb socket status id response)))

(defun format-with-status-id (finish-cb socket command header response)
  "Grab a status heder and id from resulting command."
  (declare (ignore command response))
  (let ((status (convert-to-keyword (getf header :header)))
        (id (getf header :id)))
    (funcall finish-cb socket status id)))

(defun format-with-status-count (finish-cb socket command header response)
  "Grab a status header and count from resulting command."
  (declare (ignore command response))
  (let ((status (convert-to-keyword (getf header :header)))
        (count (getf header :count)))
    (funcall finish-cb socket status count)))

(defun format-with-status (finish-cb socket command header response)
  "Just pass whatever header through as a status update, ignoring data."
  (declare (ignore command response))
  (funcall finish-cb socket (convert-to-keyword (getf header :header))))

(defun format-yaml (finish-cb socket command header response)
  "Format a YAML response from beanstalk."
  (funcall finish-cb
           socket
           (convert-to-keyword (getf header :header)) 
           (parse-beanstalk-yaml (babel:octets-to-string response :encoding :utf-8))))

;; -----------------------------------------------------------------------------
;; define all the commands and specify the function used to format the data they
;; return.
;; -----------------------------------------------------------------------------
(defcommand put (priority delay ttr)
            :sends-data t
            :format-cb #'format-with-status-id)
(defcommand use (tube) :format-cb #'format-with-status)
(defcommand reserve nil :format-cb #'format-with-status-id-data)
(defcommand reserve-with-timeout (seconds) :format-cb #'format-with-status-id-data)
(defcommand del (id) :format-cb #'format-with-status)
(defcommand release (id priority delay) :format-cb #'format-with-status)
(defcommand bury (id priority) :format-cb #'format-with-status)
(defcommand touch (id) :format-cb #'format-with-status)
(defcommand watch (tube) :format-cb #'format-with-status-count)
(defcommand ignore (tube) :format-cb #'format-with-status-count)
(defcommand peek (id) :format-cb #'format-with-status-id-data)
(defcommand peek-ready nil :format-cb #'format-with-status-id-data)
(defcommand peek-delayed nil :format-cb #'format-with-status-id-data)
(defcommand peek-buried nil :format-cb #'format-with-status-id-data)
(defcommand kick (bound) :format-cb #'format-with-status-count)
(defcommand kick-job (id) :format-cb #'format-with-status)
(defcommand stats-job (id) :format-cb #'format-yaml)
(defcommand stats-tube (tube) :format-cb #'format-yaml)
(defcommand stats nil :format-cb #'format-yaml)
(defcommand list-tubes nil :format-cb #'format-yaml)
(defcommand list-tubes-used nil :format-cb #'format-yaml)
(defcommand list-tubes-watched nil :format-cb #'format-yaml)
(defcommand quit nil)
(defcommand pause-tube (tube delay) :format-cb #'format-with-status)

