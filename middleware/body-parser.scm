;;
;; Body parsing middleware for chicken-express.
;;

(module
 middleware-body-parser
 (middleware-body-parser)
 
 (import scheme uri-common)
 (import chicken-express)
 
 (define* (middleware-body-parser)
   "Add middleware that parses the body of a request. Currently treats the body as form encoded."
   (lambda (self req res next)
     (let ((body {?req.body}))
       {!req.body (form-urldecode body)})
     (next))))
