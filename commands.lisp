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

(defmacro defcommand (fn-name args &key sends-data)
  "Makes creating new commands stupid easy."
  `(progn
     (defun ,fn-name
            ,(append args
                     (when sends-data '(data))
                     '(&key finish-cb fail-cb conn (read-timeout 5) (host "127.0.0.1") (port 11300)))
       (let ((args (list ,@args)))
         (beanstalk-command (string-downcase (string (find-command-alias ',fn-name)))
                            :args args
                            ,@(when sends-data '(:data data))
                            :finish-cb finish-cb
                            :fail-cb fail-cb
                            :conn conn
                            :read-timeout read-timeout
                            :host host
                            :port port)))
     (export ',fn-name :beanstalk-async)))

(defcommand put (priority delay ttr) :sends-data t)
(defcommand use (tube))
(defcommand reserve nil)
(defcommand reserve-with-timeout (seconds))
(defcommand del (id))
(defcommand release (id priority delay))
(defcommand bury (id))
(defcommand touch (id))
(defcommand watch (tube))
(defcommand ignore (tube))
(defcommand peek (tube))
(defcommand peek-ready nil)
(defcommand peek-delayed nil)
(defcommand peek-buried nil)
(defcommand kick (bound))
(defcommand kick-job (id))
(defcommand stats-job (id))
(defcommand stats-tube (tube))
(defcommand stats nil)
(defcommand list-tubes nil)
(defcommand list-tubes-used nil)
(defcommand list-tubes-watched nil)
(defcommand quit nil)
(defcommand pause-tube (tube delay))

