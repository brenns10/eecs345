;; The old interpreter loop, so we can run our old tests!
;; Because regression testing is important!
(define interpret
  (lambda (filename)
    (let ((err (lambda (v) (error "You can't break or continue here."))))
      (return_val (call/cc
                   (lambda (return)
                     (Mstate (parser filename) (state-new) (ctx-new return err err 'null 'null))))))))
