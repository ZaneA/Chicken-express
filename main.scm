#!/usr/bin/csi -script

;;
;; Small example of Chicken-express.
;;

(load "chicken-express.scm")
(import chicken-express)

(use colorize)

; define an app, this is the main entry-point
(define app (chicken-express))

; enable debug when in development environment (set in CHICKEN_ENV)
(@ app configure "development"
   (lambda (self)
     (@ self enable "debug")))

; wildcard route, kind of using this as a dumb template
; also, since this currently does match anything, it's impossible to 404...
; I need to look at how express handles these routes
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
       (@ res send
          "<style>
             pre { font-family: Monaco, monospace; font-size: 11px; }
             .default { color: #111; font-style: none; font-weight: normal; }
             .comment { color: #66d; }
             .variable, .special { font-style: italic; }
             .symbol, .keyword { font-weight: bold; }
             .string { color: #b42; }
             .paren1, .paren2, .paren3, .paren4, .paren5, .paren6
             { color: #666; }
           </style>")
       (@ res send (string-append "<pre>" syntax "</pre>")))))

(define *fd* #f) ; no file descriptor
(define *port* 3000)

(set! *fd* (string->number (third (command-line-arguments))))
      
(@ app listen (or *fd* *port*) *fd*)
