;;
;; middleware-logger
;;
;; Log an incoming request.
;;

(module
 middleware-logger
 (middleware-logger)

 (import scheme posix extras)
 (import chicken-express)

 (define* (middleware-logger)
   "Log an incoming request."
   (lambda (self req res next)
     (printf "[PID ~a] ~a ~a (~a)~n"
             (current-process-id)
             {@req.get "request-method"}
             {?req.path}
             {@req.get "remote-addr"})
     (next))))
