(load "interpreter.scm")

(define pass-tests '(1 2 3 4 5 6 7 8 9 10 15 16 17 18 19 20 21 22 23 24))
(define pass-vals '(150 -4 10 16 220 5 6 10 5 -39 true 100 false true
                        30 11 1106 12 16 72))
(define fail-tests '(11 12 13 14))


(define test-name
  (lambda (number)
    (string-append "tests/"
                   (let ((n (number->string number)))
                     (if (= 2 (string-length n))
                         n
                         (string-append "0" n)))
                   ".txt")))

(define run-test
  (lambda (number)
    (interpret (test-name number))))

(define run-pass-tests
  (lambda (tests values)
    (if (null? tests)
        #t
        (let ((rv (run-test (car tests))))
          (if (equal? rv (car values))
              (run-pass-tests (cdr tests) (cdr values))
              (error (string-append "test " (test-name (car tests)) " failed")))))))

(define run-fail-tests
  (lambda (tests)
    (if (null? tests)
        #t
        (begin
          (if (condition? (ignore-errors (lambda () (run-test (car tests)))))
              (run-fail-tests (cdr tests))
              (error (string-append "test " (test-name (car tests)) " failed")))))))

(write-string "Non-error tests: ")
(display (run-pass-tests pass-tests pass-vals))
(newline)
(write-string "Error tests: ")
(display (run-fail-tests fail-tests))
(newline)
