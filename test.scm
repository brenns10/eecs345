(load "interpreter.scm")

(define pass-tests car)
(define pass-vals cadr)
(define fail-tests caddr)

(define test-name
  (lambda (group number)
    (string-append group "/"
                   (let ((n (number->string number)))
                     (if (= 2 (string-length n))
                         n
                         (string-append "0" n)))
                   ".txt")))

(define run-test
  (lambda (group number)
    (interpret (test-name group number))))

(define run-pass-tests
  (lambda (group tests values)
    (if (null? tests)
        #t
        (let ((rv (run-test group (car tests))))
          (if (equal? rv (car values))
              (run-pass-tests group (cdr tests) (cdr values))
              (error (string-append "test " (test-name group (car tests)) " failed")))))))

(define run-fail-tests
  (lambda (group tests)
    (if (null? tests)
        #t
        (begin
          (if (condition? (ignore-errors (lambda () (run-test group (car tests)))))
              (run-fail-tests group (cdr tests))
              (error (string-append "test " (test-name group (car tests)) " failed")))))))

(define test
  (lambda (group)
    (let ((spec (load (string-append group "/spec.scm"))))
      (write-string "Non-error tests: ")
      (display (run-pass-tests group (pass-tests spec) (pass-vals spec)))
      (newline)
      (write-string "Error tests: ")
      (display (run-fail-tests group (fail-tests spec)))
      (newline))))
