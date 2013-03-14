#!/usr/bin/csi -script

(load "chicken-express.scm")
(use chicken-express test)

(test-begin)
(define app (chicken-express))

(test-assert "chicken-express returns non-null value"
             (not (null? app)))

(test "set works as expected" "hello"
      (begin
        (@ app set "test" "hello")
        (@ app get "test")))

(test "enable works as expected" #t
      (begin
        (@ app enable "flag")
        (@ app get "flag")))

(test "disable works as expected" #f
      (begin
        (@ app disable "flag")
        (@ app get "flag")))

(test "enabled? works as expected" #t
      (begin
        (@ app enable "flag")
        (@ app enabled? "flag")))

(test "disabled? works as expected" #t
      (begin
        (@ app disable "flag")
        (@ app disabled? "flag")))

(test "configure always runs proc when no env specified" "magic"
      (@ app configure (lambda (self) "magic")))

(setenv "CHICKEN_ENV" "testing")
(test "configure runs proc when appropriate env is configured" "magic"
      (@ app configure "testing" (lambda (self) "magic")))

(test "configure doesn't run other procs" #f
      (@ app configure "production" (lambda () "magic")))

; TODO test these with HTTP requests once socket is listening
;(test "adding middleware succeeds" #t
;      (@ app use (lambda (req res next) (print "test"))))
;(test "adding middleware with path succeeds" #t
;      (@ app use "/" (lambda (req res next) (print "test"))))
;
;(test "can register a templating engine" #t
;      (@ app engine "txt" print))
;
;(test "templating engine returns correct result" "hello, world" #f)
;
;(test "route parameter logic works" #t
;      (begin
;        (@ app param "user" (lambda (req res next id)
;                              (! res user #t)))
;        ; test properly
;        #f))

;(@ app get "/" (lambda () "Hello, World!"))
;(@ app listen 3000)
(test-end)

(test-exit)
