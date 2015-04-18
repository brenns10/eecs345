(define interpret
  (lambda (filename)
    (return_val (let* ((err (lambda (v) (error "Can't return/break/continue here.")))
                       (state (Mstate (parser filename) (state-new) (ctx-new err err err))))
                  (call/cc
                   (lambda (return)
                     (Mvalue '(funcall main) state (ctx-new return err err))))))))
