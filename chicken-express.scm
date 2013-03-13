;; Chicken-express
;;
;; Small web framework for Chicken that is modelled after Express for Node.js (http://expressjs.com).
;; Right now it also uses FastCGI and therefore should be placed behind a server such as Nginx.
;;
;; This module uses protobj for its objects, see http://wiki.call-cc.org/eggref/4/protobj for info.
;; Things starting with "%" (variables, methods, etc) are internal to the module. Everything else
;; is in theory a part of the public API exposed by the module.
;;
;; To start hacking, see the (chicken-express) method, followed by (! <app> listen) and go from there.

(module
 chicken-express
 (chicken-express) ; This is the only entry-point, call this for an application object
 
 (import scheme chicken srfi-1 srfi-13 data-structures extras)
 (require-extension srfi-69 fastcgi uri-common protobj matchable posix)
 
 (reexport protobj) ; protobj is pretty integral to the API at the moment
 
 
 ;;
 ;; Helpers
 ;;
 
 ; entry-point to the API
 (define (chicken-express)
   (% <app>)) ; return a clone of the main application object

 (define (%code-to-http-status code)
   (match code
     [200 "200 OK"]
     [301 "301 Moved Permanently"]
     [401 "401 Unauthorized"]
     [403 "403 Forbidden"]
     [404 "404 Not Found"]
     [500 "500 Internal Server Error"]
     ))
 
 (define (%obj-to-http-body obj)
   (if (integer? obj)
     (%code-to-http-status obj)
     (if (string? obj)
       obj
       (->string obj))))
 
 (define (%add-route self path proc)
   (hash-table-set!
     (? self routes) path
     (append (hash-table-ref/default (? self routes) path (list)) (list proc))))
 
 (define (%make-status-handler status)
   (lambda (self req res next)
     (@ res send status)))

 
 ;;
 ;; App object
 ;;
 
 (define <app> (%))
 
 (! <app> set ; set a key
    (lambda (self k v)
      (! self k v)))
 (! <app> get
    (match-lambda*
      [(self k) (? self k)] ; get (as in get key)
      [(self path proc) (%add-route self path proc)])) ; GET (as in verb)
 
 (! <app> post %add-route) ; TODO should actually pass the HTTP verb
 ; TODO handle next('route')
 
 (! <app> use ; mount middleware
    (match-lambda*
      [(self proc) (%add-route self "/" proc)]
      [(self path proc) (%add-route self path proc)]))
 
 (! <app> enable ; enable option
    (lambda (self k) (! self k #t)))
 (! <app> disable ; disable option
    (lambda (self k) (! self k #f)))

 (! <app> enabled? ; is option enabled?
    (lambda (self k) (eq? #t (? self k)))) ; TODO This will throw an exception is the value isn't defined
 (! <app> disabled? ; is option disabled?
    (lambda (self k) (eq? #f (? self k))))
 
 (! <app> configure ; run proc in appropriate environment
    (match-lambda*
      [(self proc) (proc self)] ; run always
      [(self env proc)          ; run only if environment matches
       (if (equal? env (get-environment-variable "CHICKEN_ENV"))
         (proc self)
         #f)]))
 
 (! <app> engine ; assign a templating engine to file extension
    (lambda (self ext proc) #f))
 
 (! <app> param ; apply logic to passed parameters
    (match-lambda*
      [(self proc) #f]
      [(self param proc) #f]))
 
 ;(! <app> locals
 
 (! <app> render ; render a template
    (match-lambda*
      [(self view proc) #f]
      [(self view options proc) #f]))
 
 (! <app> routes (make-hash-table)) ; route info
 
 ; perform routing
 (! <app> %route
    (lambda (self req res)
      (let* ((handle-404 (%make-status-handler 404))
             (route-procs (hash-table-ref/default
                            (? self routes) (? req path)
                            (list handle-404))))
        (call/cc
          (lambda (k/break) ; call break to end routing for this url
            (for-each
              (lambda (proc)
                (call/cc
                  (lambda (k/next) ; call next to continue routing
                    (proc self req res (cut k/next #t))
                    (k/break #f)))) ; if next is called in the route, this isn't reached
              route-procs))))))
 
 ; small debug helper
 (! <app> %debug
    (lambda (self . args)
      (when (@ self enabled? "debug")
        (apply printf (cons (format "[PID ~a] ~a" (current-process-id) (car args)) (cdr args))))))
 
 (! <app> listen
    (lambda (self port #!optional (is-fd? #f))
      (@ self %debug "Chicken-express application listening on fcgi://~a~n"
         (if is-fd? port (format "127.0.0.1:~a" port)))
       
      (let ((callback
              (lambda (in out err env)
                (let ((req (% <req>)) ; new request
                      (res (% <res>))) ; new response
                  (@ self %debug "Handling request: ~a~n" (env "REQUEST_URI" "/"))
                  
                  ;; Set up request object
                  (! req %uri (uri-reference (env "REQUEST_URI" "/")))
                  (! req path (string-join (map ->string (uri-path (? req %uri))) ""))

                  ; GET parameters
                  (! req query (if (env "QUERY_STRING") (form-urldecode (env "QUERY_STRING") (list))))
                  ; POST parameters
                  (! req body (if (env "HTTP_CONTENT_LENGTH")
                                (form-urldecode (fcgi-get-post-data in env))
                                (list)))

                  ;; Set up response object
                  (! res %send out)

                  ; Run user routes
                  (@ self %route req res)

                  #t)))) ; always return true or app will die!
        (if is-fd?
          ; Existing socket is provided to us
          (begin ; FCGX expects existing FD to be on 0
            (close-input-port (current-input-port)) ; close existing STDIN
            (duplicate-fileno port 0) ; duplicate provided FD to 0
            (file-close port) ; close provided FD (it is now duplicated)
            (fcgi-dynamic-server-accept-loop callback))
          ; Must create a new socket
          (fcgi-external-server-accept-loop port 0 callback)))))


 ;;
 ;; Request object
 ;;
 
 (define <req> (%))
 
 (! <req> params (list))  ; URL params as alist
 (! <req> query (list)) ; URL query as alist
 (! <req> body (list)) ; Similar to above only with POST data
 (! <req> files (%)) ; Contains info on uploaded files
 (! <req> cookies (%)) ; Cookies
 (! <req> signedCookies (%)) ; Signed Cookies
 
 (! <req> path "/")
 
 (! <req> param
    (lambda (self k)
      (object-get (? self query) k))) ; should check body and query too
 
 (! <req> route #f) ; Contains info on route
 
 (! <req> %headers #f) ; implementation detail
 (! <req> get
    (lambda (self header)
      (? (? self %headers) header))) ; Get header
 (! <req> header
    (lambda (self header)
      (@ self get header))) ; alias

 ;;
 ;; Response object
 ;;
 
 (define <res> (%))
 
 (! <res> %status 200) ; response status code
 (! <res> status ; set the response status
    (lambda (self s)
      (! self %status s)))
 
 ; helper method for sending headers
 ; invoked before sending reponse body
 (! <res> %sent-headers? #f)
 (! <res> %send-headers
    (lambda (self)
      (unless (? self %sent-headers?)
        (let ((headers '(("Content-type" "text/html"))))
          (set! headers (append headers `(("Status" ,(%code-to-http-status (? self %status))))))
          (set! headers (map (lambda (header)
                               (format "~a: ~a"
                                       (first header)
                                       (second header)))
                             headers))
          ((? self %send) (string-join headers "\r\n"))
          ((? self %send) "\r\n\r\n")
          (! self %sent-headers? #t)))))
 
 ; send proc (defaults to no-op, is replaced by FastCGI during a request)
 (! <res> %send (lambda (str) #f))
 
 ; send a status or object
 (! <res> send
    (match-lambda*
      [(self obj) ; one parameter, can be int/string/object
       (when (integer? obj)
         (@ self status obj))
       (@ self %send-headers)
       ((? self %send) (%obj-to-http-body obj))]
      [(self code obj) ; two parameters, first should be int, second can be string/object
       (@ self status code)
       (@ self %send-headers)
       ((? self %send) (%obj-to-http-body obj))]))
 )
