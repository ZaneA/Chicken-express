;; Docstrings using `extend-procedure`

(module
  chookstrings
  (define* lambda* docstring)
 
  (import scheme)
  (require-extension lolevel srfi-1)

  ;; Documented define
  (define-syntax (define* form r c)
    (if (list? (second form))
        ; (define (proc args ...) [maybe docs] body ...)
        (let* ((proc-name (take (second form) 1))
               (arguments (drop (second form) 1))
               (has-doc? (string? (third form)))
               (doc (if has-doc? (third form) #f))
               (body (drop form (if has-doc? 3 2))))
          `(,(r 'define) ,@proc-name
            (,(r 'extend-procedure)
             (,(r 'lambda) ,arguments ,@body)
             '((docstring . ,doc)))))
        ; (define var [maybe docs] body)
        (let* ((var-name (second form))
               (has-doc? (> (length form) 3))
               (doc (if has-doc? (third form) #f))
               (body (if has-doc? (fourth form) (third form))))
          `(,(r 'define) ,var-name ,body))
        ))

  (define-syntax (lambda* form r c)
    ; (lambda (args ...) [maybe docs] body ...)
    (let* ((arguments (second form))
           (has-doc? (string? (third form)))
           (doc (if has-doc? (third form) #f))
           (body (drop form (if has-doc? 3 2))))
      `(,(r 'extend-procedure)
        (,(r 'lambda) ,arguments ,@body)
        '((docstring . ,doc)))))

  (define (docstring . args)
    (if (> (length args) 1)
        (extend-procedure
         (second args)
         `((docstring . ,(first args))))
        (cdr (assoc 'docstring (procedure-data (first args))))))
  )
