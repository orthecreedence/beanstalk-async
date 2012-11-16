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

(defmacro defcommand (fn-name args &key sends-data format-cb (default-read-timeout 5))
  "Makes creating new commands stupid easy."
  `(progn
     (defun ,fn-name
            ,(append '(socket) args
                     (when sends-data '(data))
                     `(&key write-cb (read-timeout ,default-read-timeout)))
       (let ((args (list ,@args)))
         (beanstalk-command (string-downcase (string (find-command-alias ',fn-name)))
                            :args args
                            ,@(when sends-data '(:data data))
                            ,@(when format-cb `(:format-cb ,format-cb))
                            :write-cb write-cb
                            :socket socket
                            :read-timeout read-timeout)))
     (export ',fn-name :beanstalk-async)))

;; -----------------------------------------------------------------------------
;; define commands used for formatting data returned from beanstalk
;; TODO: code duplication runs rampant. fix.
;; -----------------------------------------------------------------------------
(defun format-with-status-id-data (future command header response)
  "Grab the ID from the header and the data (if any) and send them into the
   data arg of the finish-cb."
  (declare (ignore command))
  (let ((id (getf header :id))
        (status (convert-to-keyword (getf header :header))))
    (finish future status id response)))

(defun format-with-status-id (future command header response)
  "Grab a status heder and id from resulting command."
  (declare (ignore command response))
  (let ((status (convert-to-keyword (getf header :header)))
        (id (getf header :id)))
    (finish future status id)))

(defun format-with-count (future command header response)
  "Grab a status header and count from resulting command."
  (declare (ignore command response))
  (let ((count (getf header :count)))
    (finish future count)))

(defun format-with-status-count (future command header response)
  "Grab a status header and count from resulting command."
  (declare (ignore command response))
  (let ((status (convert-to-keyword (getf header :header)))
        (count (getf header :count)))
    (finish future status count)))

(defun format-with-status (future command header response)
  "Just pass whatever header through as a status update, ignoring data."
  (declare (ignore command response))
  (finish future (convert-to-keyword (getf header :header))))

(defun format-yaml (future command header response)
  "Format a YAML response from beanstalk."
  (declare (ignore command))
  (finish future
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
(defcommand reserve nil
            :format-cb #'format-with-status-id-data
            :default-read-timeout nil)
(defcommand reserve-with-timeout (timeout)
            :format-cb #'format-with-status-id-data
            :default-read-timeout nil)
(defcommand del (id) :format-cb #'format-with-status)
(defcommand release (id priority delay) :format-cb #'format-with-status)
(defcommand bury (id priority) :format-cb #'format-with-status)
(defcommand touch (id) :format-cb #'format-with-status)
(defcommand watch (tube) :format-cb #'format-with-count)
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

