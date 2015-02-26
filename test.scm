(load "interpreter.scm")

(define testnum car)
(define expect cadr)

(define test-name
  (lambda (group number)
    (string-append group "/"
                   (let ((n (number->string number)))
                     (if (= 2 (string-length n))
                         n
                         (string-append "0" n)))
                   ".txt")))

(define run-test
  (lambda (group number expected)
    (let ((rv (ignore-errors (lambda () (interpret (test-name group number))))))
      (cond
       ((and (condition? rv) (eq? expected 'error))
        (write-string (string-append (test-name group number) ": Pass\n")))
       ((eq? rv expected)
        (write-string (string-append (test-name group number) ": Pass\n")))
       ((condition? rv)
        (begin
          (write-string (string-append (test-name group number)
                                       ": Fail: expected "))
          (display expected)
          (write-string ", got error \"")
          (write-condition-report rv (current-output-port))
          (write-string "\"\n")))
       (else (begin
               (write-string (string-append (test-name group number)
                             ": Fail: expected "))
               (display expected)
               (write-string ", got ")
               (display rv)
               (newline)))))))

(define run-tests
  (lambda (group list)
    (map (lambda (v) (run-test group (testnum v) (expect v))) list)
    'tests_complete))

(define test
  (lambda (group)
    (run-tests group (load (string-append group "/spec.scm")))))

