;;
;; bodyParser middleware for chicken-express.
;;

(module
 middleware-body-parser
 (middleware-body-parser)
 
 (import chicken-express)
 
 (define* (middleware-body-parser)
   "Add middleware that parses the body of a request."
   #f)
 )
