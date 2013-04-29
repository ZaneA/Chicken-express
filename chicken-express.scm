;;
;; Chicken-express
;;
;; Small web framework for Chicken that is modelled after Express for
;; Node.js (http://expressjs.com). Right now it also uses FastCGI and
;; therefore should be placed behind a server such as Nginx.
;;
;; This module uses protobj for its objects, see
;; http://wiki.call-cc.org/eggref/4/protobj for info. Things starting
;; with "%" (variables, methods, etc) are internal to the module.
;; Everything else is in theory a part of the public API exposed by
;; the module.
;;
;; To start hacking, see the (chicken-express) method, followed by
;; (! <app> listen) and go from there.
;;

(include "documented-procedures.scm")
 
; Read-syntax enabling protobj shortcut.
; Replaces {@obj.method arg1 .. argn} with (@ obj method arg1 .. argn)
; Base for this is largely taken from
; http://wiki.call-cc.org/set-read-syntax-example
(set-read-syntax! #\{
   (lambda (port)
     (let loop ((c (peek-char port)) (exps (list)))
       (cond ((eof-object? c)
              (error "EOF encountered while parsing { ... } clause"))
             ((char=? c #\})
              (read-char port) ; discard
              (let* ((exps (reverse exps))
                     ; "@" "obj" "method"
                     (objs (string-split (symbol->string (car exps)) "."))
                     ; @ ! ?
                     (type (string->symbol (substring (car objs) 0 1)))
                     ; obj
                     (obj (string->symbol (substring (car objs) 1)))
                     ; (method)
                     (props (map string->symbol (cdr objs)))
                     ; (arg1 .. argn)
                     (args (cdr exps)))
                (cond ((eq? type '+) ; append
                       `(! ,obj ,@props (append (? ,obj ,@props) (list ,@args))))
                      (else
                       `(,type ,obj ,@props ,@args)))))
             ((char-whitespace? c)
              (read-char port) ; discard
              (loop (peek-char port) exps))
             (else
              (let ((exp (read port)))
                (loop (peek-char port)
                      (cons exp exps))))))))

(module
 chicken-express
 (chicken-express) ; This is the only entry-point, call this for an application object
 
 (import scheme chicken srfi-1 srfi-13 data-structures extras)
 (require-extension srfi-69 fastcgi uri-common protobj matchable posix irregex)
 (import documented-procedures)
 
 (reexport documented-procedures protobj) ; protobj is pretty integral to the API at the moment
 
 ;;
 ;; Helpers
 ;;
 
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
   (cond ((integer? obj)
          (%code-to-http-status obj))
         ((string? obj)
          obj)
         (else
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
   "Add a route to the middleware stack."
   (%add-middleware self "/" (%make-route-middleware path verb proc)))

 (define* (%add-middleware self mount-path proc)
   "Add a middleware to the stack at `mount-path`."
   {+self.middleware (cons mount-path proc)})

 (define* (%make-route-middleware route-spec verb proc)
   "Return a middleware procedure suitable for matching the provided `route-spec`."
   (lambda (self req res next)
     ; if verb (GET/POST/...) matches
     (if (or (eq? verb 'ANY)
             (eq? (string->symbol {@req.get "request-method"}) verb))
       (let* ((route-regex (%route-to-regex route-spec))
              (matches (irregex-match route-regex {?req.path}))
              (match-names (if matches (irregex-match-names matches) #f))
              (match-values (if match-names
                               (map (lambda (match)
                                      (cons (car match) (irregex-match-substring matches (cdr match))))
                                    match-names)
                              (list))))
         (if matches
            (begin
              {!req.params match-values}
              (proc self req res next))
           (next)))
       (next))))

 
 ;;
 ;; App object
 ;;
 
 (define* <app>
   "The main app object."
   (%))
 
 (! <app> set ; set a key
    (lambda (self k v)
      {!self.k v}))
 (! <app> get
    (match-lambda*
      [(self k) {?self.k}] ; get (as in get key)
      [(self path proc) (%add-route self path proc 'GET)])) ; GET (as in verb)
 
 (! <app> post (cut %add-route <> <> <> 'POST))
 (! <app> all (cut %add-route <> <> <> 'ANY))
 ; TODO handle next('route')
 
 (! <app> use ; mount middleware
    (match-lambda*
      [(self proc) (%add-middleware self "/" proc)]
      [(self path proc) (%add-middleware self path proc)]))
 
 (! <app> enable ; enable option
    (lambda (self k) {!self.k #t}))
 (! <app> disable ; disable option
    (lambda (self k) {!self.k #f}))

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
 (! <app> middleware (list))

 ; perform routing
 (! <app> %route
    (lambda (self req res)
      (let* ((path {?req.path})
             (middleware-list (filter (lambda (middleware)
                                        (string-prefix? (car middleware) path))
                                      {?self.middleware})))
        (call/cc
          (lambda (k/break) ; call break to end routing for this url
            (for-each
              (lambda (middleware)
                (match-let (((mount-path . proc) middleware))
                  (call/cc
                    (lambda (k/next) ; call next to continue routing
                      {!req.path path} ; reset path
                      (proc self req res (cut k/next #t))

                      ; end routing for this url. if (next) is called
                      ; from the proc then this isn't reached
                      (k/break #f)))))
              middleware-list))))))
 
 ; small debug helper
 (! <app> %debug
    (lambda (self . args)
      (when {@self.enabled? "debug"}
        (apply printf (cons (format "[PID ~a] ~a" (current-process-id) (car args)) (cdr args))))))
 
 (! <app> listen
    (lambda (self port #!optional (is-fd? #f))
      {@self.%debug "Chicken-express application listening on fcgi://~a~n"
         (if is-fd? port (format "127.0.0.1:~a" port))}
       
      (let ((callback
              (lambda (in out err env)
                (let ((req (% <req>)) ; new request
                      (res (% <res>))) ; new response
                  {@self.%debug "New connection...~n"}
                  
                  ;; Set up request object
                  {!req.%uri (uri-reference (env "REQUEST_URI" "/"))}
                  ; ugly way to turn the path into a string...
                  {!req.path (string-join (filter string? (uri-path (? req %uri))) "/" 'prefix)}

                  ; Add environment
                  {!req.%headers (map (lambda (header)
                                        (cons (string-downcase (string-translate (car header) #\_ #\-))
                                              (cdr header)))
                                      (env))}

                  ; GET parameters
                  (let ((env-get (env "QUERY_STRING")))
                    {!req.query (if (and env-get (not (string-null? env-get)))
                                   (form-urldecode env-get) (list))})
                  ; POST parameters
                  (let ((env-post (env "HTTP_CONTENT_LENGTH")))
                    {!req.body (if (and env-post (not (string-null? env-post)))
                                  (fcgi-get-post-data in env)
                                 "")})

                  ;; Set up response object
                  {!res.%send out}

                  ; Run user routes
                  {@self.%route req res}

                  (unless {?res.%sent-headers?}
                    {@res.send 404})
                  
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
 
 ; get a parameter, can be from URL params, query, or POST body
 (! <req> param
    (lambda (self k #!optional (default #f))
      (let ((param (or (assoc k {?self.params})
                       (assoc k {?self.query})
                       (assoc k {?self.body}))))
        (if param
          (cdr param)
          default))))
 
 (! <req> route #f) ; Contains info on route
 
 (! <req> %headers (list)) ; implementation detail
 (! <req> get
    (lambda (self header)
      (let ((headers {?self.%headers})
            (header (string-downcase header)))
        (alist-ref header headers string=?))))
 (! <req> header
    (lambda (self header)
      {@self.get header})) ; alias

 ;;
 ;; Response object
 ;;
 
 (define <res> (%))
 
 (! <res> %status 200) ; response status code
 (! <res> status ; set the response status
    (lambda (self s)
      {!self.%status s}))
 
 ; helper method for sending headers
 ; invoked before sending reponse body
 (! <res> %sent-headers? #f)
 (! <res> %send-headers
    (lambda (self)
      (unless {?self.%sent-headers?}
        (let ((headers '(("Content-type" "text/html")))
              (send {?self.%send}))
          (set! headers (append headers `(("Status" ,(%code-to-http-status (? self %status))))))
          (set! headers (map (lambda (header)
                               (format "~a: ~a"
                                       (first header)
                                       (second header)))
                             headers))
          (send (string-join headers "\r\n"))
          (send "\r\n\r\n")
          {!self.%sent-headers? #t}))))
 
 ; send proc (defaults to no-op, is replaced by FastCGI during a request)
 (! <res> %send identity)
 
 ; send a status or object
 (! <res> send
    (match-lambda*
      [(self obj) ; one parameter, can be int/string/object
       (when (integer? obj)
         {@self.status obj})
       {@self.%send-headers}
       ({?self.%send} (%obj-to-http-body obj))]
      [(self code obj) ; two parameters, first should be int, second can be string/object
       {@self.status code}
       {@self.%send-headers}
       ({?self.%send} (%obj-to-http-body obj))]))
 )
