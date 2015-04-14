(define interpret
  (lambda (filename)
    (return_val (let* ((err (lambda (v) (error "Can't return/break/continue here.")))
                       (state (Mstate (parser filename) (state-new) err err err)))
                  (call/cc
                   (lambda (return)
                     (Mvalue '(funcall main) state return err err)))))))
