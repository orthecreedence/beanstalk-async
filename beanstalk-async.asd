(asdf:defsystem beanstalk-async
  :depends-on (#:cl-async #:babel #:split-sequence)
  :components
  ((:file "beanstalk")
   (:file "commands" :depends-on ("beanstalk"))))

