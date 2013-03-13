#!/usr/bin/csi -script

(load "chicken-express.scm")
(import chicken-express)

(define app (chicken-express))

(@ app configure "development"
   (lambda (self)
     (@ self enable "debug")))

(@ app get "/"
   (lambda (self req res next)
     (@ res send (format "Parameters: <pre>~a</pre>~n" (? req query)))
     (next))) ; pass to next handler

(@ app get "/"
   (lambda (self req res next)
     (@ res send "More text!~n")))
   
(@ app get "/test"
   (lambda (self req res next)
     (@ res send (format "Current path: <b>~a</b><br />~n" (? req path)))))

(define *fd* #f) ; no file descriptor
(define *port* 3000)

(set! *fd* (string->number (third (command-line-arguments))))
      
(@ app listen (or *fd* *port*) *fd*)
