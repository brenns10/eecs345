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
    (let ((rv (with-handlers ([exn:fail? (lambda (e) #f)])
                             (interpret (test-name group number)))))
      (cond
       ((and (not rv) (eq? expected 'error))
        (display (string-append (test-name group number) ": Pass\n")))
       ((eq? rv expected)
        (display (string-append (test-name group number) ": Pass\n")))
       ((not rv)
        (begin
          (display (string-append (test-name group number)
                                  ": Fail: expected "))
          (display expected)
          (display ", got error.\n")))
       (else (begin
               (display (string-append (test-name group number)
                                       ": Fail: expected "))
               (display expected)
               (display ", got ")
               (display rv)
               (newline)))))))

(define run-tests
  (lambda (group list)
    (map (lambda (v) (run-test group (testnum v) (expect v))) list)
    'tests_complete))

(define test
  (lambda (group)
    (run-tests group (load (string-append group "/spec.scm")))))

