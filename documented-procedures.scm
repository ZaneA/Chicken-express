;; Documented procedures

(module
  doc-table
  (get-documentation-hash-table)

  (import scheme)
  (require-extension srfi-69)

  (define get-documentation-hash-table
    (let ((table (make-hash-table)))
      (lambda ()
        table))))

(module
  documented-procedures
  (get-documentation-hash-table define* doc walk-documentation)
 
  (import scheme chicken srfi-1 data-structures extras)
  (require-extension srfi-69)

  ; Needs to be outside for some reason
  (import doc-table)
  (import-for-syntax doc-table)

  ;; Documented define
  (define-syntax (define* form r c)
    (let* ((args-form (second form))
           (proc-name (if (list? args-form)
                        (first args-form)
                        args-form))
           (doc (third form))
           (body (drop form 3)))
      (hash-table-set! (get-documentation-hash-table) proc-name (list doc body))
      `(,(r 'define) ,args-form ,@body)))

  (define (doc proc #!optional (source #f))
    (let ((docs (hash-table-ref/default
                  (get-documentation-hash-table)
                  proc
                  (list (format "No documentation for proc ~a" proc) '())))
          (which (if source cadr car)))
      (which docs)))

  (define (walk-documentation proc)
    (hash-table-walk (get-documentation-hash-table) proc))
  )
