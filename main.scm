#!/usr/bin/csi -script

;;
;; Small example of Chicken-express.
;;
;; I start with:
;; CHICKEN_ENV=development einhorn -c chicken-express csi \
;; -script main.scm -- --fd srv:127.0.0.1:3000,so_reuseaddr
;;

(load "chicken-express.scm")
(import chicken-express)

; for syntax highlighting
(use colorize)

; define an app, this is the main entry-point
(define app (chicken-express))

; enable debug when in development environment (set in CHICKEN_ENV)
(@ app configure "development"
   (lambda (self)
     (@ self enable "debug")))

; serve static files from /public/
; bit of a hack right now...
(@ app use "/public/"
   (lambda (self req res next)
     (let ((filename (string-append (current-directory) (? req path))))
       (if (regular-file? filename)
         ; should actually use X-Accel-Redirect header (for Nginx)
         (@ res send (read-all filename))
         (next)))))

; wildcard route, kind of using this as a dumb template
(@ app get "*"
   (lambda (self req res next)
     (@ res send "<h1>Welcome to Chicken-express!</h1>")
     (next))) ; pass to next handler

; home route
(@ app get "/"
   (lambda (self req res next)
     (let ((name (@ req param 'name)))
       (@ res send "<a href=\"/test/hello+world\">/test route</a>")
       (if name
         (@ res send (format "<p>Hello, <b>~a</b>!</p>" (if (string-null? name) "World" name)))
         (@ res send
            "<form action=\"/\" method=\"post\">
               Name:
               <input type=\"text\" name=\"name\" />
               <input type=\"submit\" value=\"GO\" />
             </form>"))
       (next))))
   
; test route
; displays a parameter pulled from the URL query
(@ app get "/test/:name"
   (lambda (self req res next)
     (let ((path (format "Current path is <b>~a</b><br />~n" (? req path)))
           (parameter (@ req param 'name)))
       (@ res send (format "Parameter is \"~a\"<br />~n" parameter))
       (@ res send path)
       (next))))

; show example source
(@ app get "*"
   (lambda (self req res next)
     (let ((syntax (html-colorize 'scheme (read-all "main.scm"))))
       (@ res send "<link rel=\"stylesheet\" type=\"text/css\" href=\"/public/style.css\" />")
       (@ res send (string-append "<pre>" syntax "</pre>")))))


; handle command-line
(define *fd* #f) ; no file descriptor
(define *port* 3000)

(set! *fd* (string->number (third (command-line-arguments))))
      
(@ app listen (or *fd* *port*) *fd*)
