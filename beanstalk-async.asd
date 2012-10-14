(asdf:defsystem beanstalk-async
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.0.8"
  :description "Asynchronous beanstalkd driver for Common Lisp."
  :depends-on (#:cl-async #:babel #:split-sequence)
  :components
  ((:file "beanstalk")
   (:file "commands" :depends-on ("beanstalk"))))

