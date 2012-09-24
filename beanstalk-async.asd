(asdf:defsystem beanstalk-async
  :depends-on (#:cl-async #:flexi-streams #:split-sequence)
  :components
  ((:file "beanstalk")
   (:file "commands" :depends-on ("beanstalk"))))

