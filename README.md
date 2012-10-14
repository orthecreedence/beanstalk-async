beanstalk-async - Asynchronous beanstalkd driver for Common Lisp
================================================================
Requires [cl-async](https://github.com/orthecreedence/cl-async).

Commands
--------
All commands take the following keyword arguments, and so are not documented
individually for each command (they will be represented as "..." in the function
definitions).

- `:finish-cb` - The callback that is run when the command completes
successfully. `finish-cb` takes different arguments depending on what command is
run, which will be documented for each command. However, every `finish-cb` does
take a `socket` parameter as the first argument, which can be used to send more
commands over the same socket.
- `:event-cb` - Callback that's invoked when errors or events occurs while
processing a command. This can be a connection being closed, beanstalkd
returning an error, etc.
- `:write-cb` - This callback is called when the data being sent to beanstalkd
has finished sending (ie the command data was flushed out to the socket). It can
be useful to close the socket directly after sending data.
- `:socket` - When given the `socket` passed into `finish-cb`, the next command
will be written onto that socket instead of opening a new one.
- `:read-timeout` - How many seconds to wait for data on a socket before timing
out. This defaults to 5 for all commands except the `reserve` commands, which
default to `nil` (no timeout).
- `:host` - The host to send the command to. Ignored if `:socket` is given.
- `:port` - The port to send the command to. Ignored if `:socket` is given.

#### Command reference
- [beanstalk-command](#beanstalk-command)
- [put](#put)
- [use](#use)
- [reserve](#reserve)
- [reserve-with-timeout](#reserve-with-timeout)
- [del](#del)
- [release](#release)
- [bury](#bury)
- [touch](#touch)
- [watch](#watch)
- [ignore](#ignore)
- [peek](#peek)
- [peek-ready](#peek-ready)
- [peek-delayed](#peek-delayed)
- [peek-buried](#peek-buried)
- [kick](#kick)
- [kick-job](#kick-job)
- [stats-job](#stats-job)
- [stats-tube](#stats-tube)
- [stats](#stats)
- [list-tubes](#list-tubes)
- [list-tubes-used](#list-tubes-used)
- [list-tubes-watched](#list-tubes-watched)
- [quit](#quit)
- [pause-tube](#pause-tube)

### beanstalk-command
The low-level command that all other commands are built on top of. Supports
opening a new connection, or writing data onto an existing connect (socket) if
it's passed in via the `:socket` keyword. The socket is passed in to all
`finish-cb` arguments, allowing more commands to be run on the same socket once
a command finishes.

```common-lisp
;; definition
(beanstalk-command command &key args data ...)

;; examples
(beanstalk-command "delete"
                   :args '(13)
                   :finish-cb (lambda (socket response)
                                ...)
                   :event-cb (lambda (err)
                               (format t "event: ~a~%" err)))
```

`beanstalk-command` isn't really meant for direct usage, although it's exposed
in case you need it.

### put
Add a job to the queue.

```common-lisp
;; definition
(put priority delay ttr data &key ...)

;; example
(put 1024 0 30 "get a job"
  :finish-cb (lambda (sock status id) ...)
  :event-cb (lambda (ev) (format t "event while putting: ~a~%" ev)))

;; finish-cb definition
(lambda (socket status id) ...)
```

### use
Use a tube.

```common-lisp
;; definition
(use tube &key ...)

;; example
(use "instant-messages"
  :finish-cb (lambda (sock status)
               (put 1024 0 30 "LOLOL!!!!!"
                 :finish-cb (lambda (sock status id)
                              ...)
                 :socket sock)))

;; finish-cb definition
(lambda (socket status) ...)
```

### reserve
Reserve an item from the queue. Doesn't run its `finish-cb` until a message is
available on the watched tubes.

```common-lisp
;; definition
(reserve &key ...)

;; example
(reserve
  :finish-cb (lambda (sock status id data)
               ...))

;; finish-cb definition
(lambda (socket status id data) ...)
```

`data` in the `format-cb` is a byte-array, and can be converted to a string via
`babel:octets-to-string`.

### reserve-with-timeout
Reserve an item from the queue, but if a message doesn't become available on the
watched tubes before `timeout` seconds passes, this will call `finish-cb` with
`:timed-out` in the status.

```common-lisp
;; definition
(reserve-with-timeout timeout &key ...)

;; example
(reserve-with-timeout 5
  :finish-cb (lambda (sock status id data)
               ...))

;; finish-cb definition
(lambda (socket status id data) ...)
```

`data` in the `format-cb` is a byte-array, and can be converted to a string via
`babel:octets-to-string`.

### del
Delete a message in the used tube.

```common-lisp
;; definition
(del id &key ...)

;; example
(watch "jobs"
  :finish-cb (lambda (sock status count)
               (reserve
                 :finish-cb (lambda (sock status id data)
                              (process-job id data)
                              (del id
                                :finish-cb (lambda (sock status)
                                             (quit :socket sock))
                                :socket sock))
                 :socket sock)))

;; finish-cb definition
(lambda (socket status) ...)
```

### release
Release a job back inot the queue.

```common-lisp
;; definition
(release id priority delay &key ...)

;; example
(reserve
  :finish-cb (lambda (sock status id data)
               ;; OH NO error processing job, release it
               (release id 1024 10
                 :finish-cb (lambda (sock status)
                              ...)
                 :socket sock)))

;; finish-cb definition
(lambda (socket status) ...)
```

### bury
Bury a message.

```common-lisp
;; definition
(bury id priority &key ...)

;; example
(reserve
  :finish-cb (lambda (sock status id data)
               (bury id 1024
                 :finish-cb (lambda (sock status)
                              (quit :socket sock))
                 :socket sock)))

;; finish-cb definition
(lambda (socket status) ...)
```
