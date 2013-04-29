#!/usr/bin/csi -script
;;
;; Small example of Chicken-express.
;;
;; I start with:
;; CHICKEN_ENV=development einhorn -c chicken-express ./example.scm --fd srv:127.0.0.1:3000,so_reuseaddr
;;

; load chicken-express
(load "chicken-express.scm")
(import chicken-express)

; load middleware
(load "middleware/logger.scm")
(load "middleware/static.scm")
(load "middleware/favicon.scm")
(load "middleware/body-parser.scm")
(import middleware-logger)
(import middleware-static)
(import middleware-favicon)
(import middleware-body-parser)

; for syntax highlighting and command-line parsing
(use colorize matchable)

; handle command-line
(define *fd* #f) ; no file descriptor
(define *port* 3000)

(let loop ((args (command-line-arguments)))
  (match args
    [("--help" rest ...) (print "Usage: [--fd <file-descriptor>] [--port <port>] [--help]") (exit)]
    [("--fd" fd rest ...) (set! *fd* (string->number fd)) (loop rest)]
    [("--port" port rest ...) (set! *port* (string->number port)) (loop rest)]
    [() #f]
    [=> (loop (cdr args))]))

; define an app, this is the main entry-point
(define app (chicken-express))

; enable debug when in development environment (set in CHICKEN_ENV)
{@app.configure "development"
   (lambda (self)
     {@self.enable "debug"})}

; enable middleware
{@app.use (middleware-logger)}
{@app.use (middleware-static "./public/")}
{@app.use (middleware-favicon)}
{@app.use (middleware-body-parser)}

; wildcard route, kind of using this as a dumb template
{@app.get "*"
   (lambda (self req res next)
     {@res.send "<h1>Welcome to Chicken-express!</h1>"}
     {@res.send (format "<pre>~a</pre>" {@req.get "remote-addr"})}
     (next))} ; pass to next handler

; home route
{@app.all "/"
   (lambda (self req res next)
     (let ((name {@req.param 'name}))
       {@res.send "<a href=\"/test/hello+world\">/test route</a>"}
       (if name
         {@res.send (format "<p>Hello, <b>~a</b>!</p>" (if (string-null? name) "World" name))}
         {@res.send
            "<form action=\"/\" method=\"post\">
               Name:
               <input type=\"text\" name=\"name\" />
               <input type=\"submit\" value=\"GO\" />
             </form>"})
       (next)))}
   
; test route
; displays a parameter pulled from the URL query
{@app.get "/test/:name"
   (lambda (self req res next)
     (let ((path (format "Current path is <b>~a</b><br />~n" {?req.path}))
           (parameter {@req.param 'name}))
       {@res.send (format "Parameter is \"~a\"<br />~n" parameter)}
       {@res.send path}
       (next)))}

; show example source
{@app.get "*"
   (lambda (self req res next)
     (let ((syntax (html-colorize 'scheme (read-all "example.scm"))))
      {@res.send "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />"}
      {@res.send (string-append "<pre>" syntax "</pre>")}))}

; run the application!
{@app.listen (or *fd* *port*) *fd*}
