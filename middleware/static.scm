;;
;; middleware-static
;;
;; Serve static files with chicken-express.
;;

(module
 middleware-static
 (middleware-static)

 (import scheme posix utils)
 (import chicken-express)

 (define* (middleware-static path)
   "Middleware that intercepts static requests and serves a file directly."
   (lambda (self req res next)
     (let ((filename (string-append path {?req.path})))
       (if (regular-file? filename)
          ; should actually use X-Accel-Redirect header (for Nginx)
          {@res.send (read-all filename)}
         (next))))))
