(define testnum car)
(define expect cadr)
(define pairs cdr)
(define class car)

(define test-name
  (lambda (group number)
    (string-append group "/"
                   (let ((n (number->string number)))
                     (if (= 2 (string-length n))
                         n
                         (string-append "0" n)))
                   ".txt")))

;; Run an old-style test.
(define run-test-noclass
  (lambda (group number expected)
    (let ((rv (with-handlers ([exn:fail? (lambda (e) 'error)])
                             (interpret (test-name group number)))))
      (if (eq? rv expected)
          (display (string-append (test-name group number) ": Pass\n"))
          ;; Else
          (begin
            (display (string-append (test-name group number)
                                    ": Fail: expected "))
            (display expected)
            (display ", got ")
            (display rv)
            (newline))))))

;; Run a test that has to have a class specified.
(define run-test-class
  (lambda (group number class expected)
    (let ((rv (with-handlers ([exn:fail? (lambda (e) 'error)])
                             (interpret (test-name group number) class))))
      (if (eq? rv expected)
          (begin
            (display (test-name group number))
            (display " (")
            (display class)
            (display ") : Pass\n"))
          ;; Else
          (begin
            (display (test-name group number))
            (display " (")
            (display class)
            (display ") : Fail: expected ")
            (display expected)
            (display ", got ")
            (display rv)
            (newline))))))

;; Run a list of class/expectation pairs.
(define run-tests-class
  (lambda (group number tests)
    (map (lambda (v) (run-test-class group number (class v) (expect v))) tests)))

;; Run a test, given the spec entry for it, and its group.
(define run-test
  (lambda (group v)
    (if (list? (expect v))
        (run-tests-class group (testnum v) (pairs v))
        (run-test-noclass group (testnum v) (expect v)))))

;; Run the tests in a spec.
(define run-tests
  (lambda (group list)
    (map (lambda (v) (run-test group v)) list)
    'tests_complete))

;; Load supplementary files, in order to support older interpreters.
(define init
  (lambda (type)
    (begin
      (load "interpreter.scm")
      (cond
       ((equal? type "simple")
        (begin
          (load "simpleParser.scm") ; Overwrite the bindings of functionParser
          (load "simpleInterpreter.scm")) ; Old interpreter loop
        #t)
       ((equal? type "function")
        (begin
          (load "functionParser.scm")
          (load "functionInterpreter.scm")))
       (else #t)))))

;; Main function - test a group name.
(define test
  (lambda (group)
    (let ((spec (load (string-append group "/spec.scm"))))
      (begin
        (init (car spec))
        (run-tests group (cdr spec))))))

