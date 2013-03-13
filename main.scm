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

; home route
(@ app get "/"
   (lambda (self req res next)
     (@ res send "<p>Welcome to Chicken-express!</p>")
     (next))) ; pass to next handler

; second home route
(@ app get "/"
   (lambda (self req res next)
     (let ((parameters (? req body)))
       (if (not (null-list? parameters))
         (@ res send (format "<p>Parameters: <pre>~a</pre></p>" parameters))
         (@ res send
            "<form action=\"/\" method=\"post\">
               Name:
               <input type=\"text\" name=\"name\" />
               <input type=\"submit\" value=\"GO\" />
             </form>"))
       (next))))

; show example source
(@ app get "/"
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
   
; test route
(@ app get "/test"
   (lambda (self req res next)
     (let ((path (format "Current path is <b>~a</b><br />~n" (? req path))))
       (@ res send path))))

(define *fd* #f) ; no file descriptor
(define *port* 3000)

(set! *fd* (string->number (third (command-line-arguments))))
      
(@ app listen (or *fd* *port*) *fd*)
