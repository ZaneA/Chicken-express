;;
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
;;

(include "documented-procedures.scm")

(module
 chicken-express
 (chicken-express) ; This is the only entry-point, call this for an application object
 
 (import scheme chicken srfi-1 srfi-13 data-structures extras)
 (require-extension srfi-69 fastcgi uri-common protobj matchable posix irregex)
 (import documented-procedures)
 
 (reexport protobj) ; protobj is pretty integral to the API at the moment
 
 
 ;;
 ;; Helpers
 ;;
 
 ; entry-point to the API
 (define* (chicken-express)
   "Entry-point to the API."
   (% <app>)) ; return a clone of the main application object

 (define* (%code-to-http-status code)
   "Convert a status code into a reply."
   (match code
     [200 "200 OK"]
     [301 "301 Moved Permanently"]
     [401 "401 Unauthorized"]
     [403 "403 Forbidden"]
     [404 "404 Not Found"]
     [500 "500 Internal Server Error"]
     ))
 
 (define* (%obj-to-http-body obj)
   "Convert an object into a suitable reply."
   (if (integer? obj)
     (%code-to-http-status obj)
     (if (string? obj)
       obj
       (->string obj))))
 
 (define* (%route-to-regex route)
   "(Sloppily) convert a basic route format into regular expression."
   (let ((replacements
           `(("\\/"             . "\\/")
             ("\\*"             . ".*?")
             (":([[:alpha:]]+)" . ,(lambda (match)
                                     (format
                                       "(?<~a>.*?)"
                                       (irregex-match-substring match 1)))))))
     (for-each
       (lambda (n)
         (set! route (irregex-replace/all (car n) route (cdr n))))
       replacements)
     route))
 
 (define* (%add-route self path proc verb)
   "Add a route."
   (let ((path (%route-to-regex path)))
     (! self routes (append (? self routes) (list (cons path proc))))))
 
 (define* (%wildcard-route? route)
   "Test if this route is a wildcard route, which is handled specially."
   (string=? (car route) ".*?"))
 
 ; make default routes for an HTTP status
 ; this is used to make a 404 route when no routes match
 (define* (%make-status-handler status)
   "Create a route to handle a status code."
   (cons #f (lambda (self req res next)
              (@ res send status))))

 
 ;;
 ;; App object
 ;;
 
 (define* <app>
   "The main app object."
   (%))
 
 (! <app> set ; set a key
    (lambda (self k v)
      (! self k v)))
 (! <app> get
    (match-lambda*
      [(self k) (? self k)] ; get (as in get key)
      [(self path proc) (%add-route self path proc 'get)])) ; GET (as in verb)
 
 (! <app> post (cut %add-route <> <> <> 'post))
 (! <app> all (cut %add-route <> <> <> 'all))
 ; TODO handle next('route')
 
 (! <app> use ; mount middleware
    (match-lambda*
      [(self proc) (%add-route self "/*" proc 'middleware)]
      [(self path proc) (%add-route self (string-append path "*") proc 'middleware)]))
 
 (! <app> enable ; enable option
    (lambda (self k) (! self k #t)))
 (! <app> disable ; disable option
    (lambda (self k) (! self k #f)))

 (! <app> enabled? ; is option enabled?
    (lambda (self k) ; a bit of a hack here to return false if value is missing
      (handle-exceptions ex #f (eq? #t (? self k)))))
 (! <app> disabled? ; is option disabled?
    (lambda (self k)
      (handle-exceptions ex #f (eq? #f (? self k)))))
 
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
 
 (! <app> routes (list)) ; route info
 
 ; perform routing
 (! <app> %route
    (lambda (self req res)
      (let* ((path (? req path))
             (handle-404 (%make-status-handler 404))
             (route-procs (filter (lambda (route)
                                    (irregex-match (car route) path))
                                  (? self routes)))
             (route-procs (if (null-list?
                                (filter (complement %wildcard-route?) route-procs))
                            (list handle-404)
                            route-procs)))
        (call/cc
          (lambda (k/break) ; call break to end routing for this url
            (for-each
              (lambda (route)
                (call/cc
                  (lambda (k/next) ; call next to continue routing
                    
                    ; this sets up parameters for a route, this can be highly optimised
                    ; by simply avoiding a second match (we're already matching above)
                    (when (car route)
                      (let* ((matches (irregex-match (car route) path))
                             (match-names (irregex-match-names matches))
                             (match-values (if match-names
                                             (map (lambda (match)
                                                    (cons (car match) (irregex-match-substring matches (cdr match))))
                                                  match-names)
                                             (list))))
                        (! req params match-values)))

                    ; call the user proc
                    ((cdr route) self req res (cut k/next #t))
                    
                    ; end routing for this url.
                    ; if (next) is called from the proc then this isn't reached
                    (k/break #f))))
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
                  ; ugly way to turn the path into a string...
                  (! req path (string-join (filter string? (uri-path (? req %uri))) "/" 'prefix))

                  ; GET parameters
                  (let ((env-get (env "QUERY_STRING")))
                    (! req query (if (and env-get (not (string-null? env-get)))
                                   (form-urldecode env-get) (list))))
                  ; POST parameters
                  ; TODO Move into middlware/body-parser.scm
                  (let ((env-post (env "HTTP_CONTENT_LENGTH")))
                    (! req body (if (and env-post (not (string-null? env-post)))
                                  (form-urldecode (fcgi-get-post-data in env)) (list))))

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
 
 (! <req> param ; get a parameter, can be from URL params, query, or POST body
    (lambda (self k #!optional (default #f))
      (let ((param (or (assoc k (? self params))
                       (assoc k (? self query))
                       (assoc k (? self body)))))
        (if param
          (cdr param)
          default))))
 
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
