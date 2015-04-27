(define interpret
  (lambda (filename)
    (return_val (let* ((err (lambda (v) (error "Can't return/break/continue here.")))
                       (state (Mstate (parser filename) (state-new) (ctx-default))))
                  (call/cc
                   (lambda (return)
                     (Mvalue '(funcall main) state (ctx-return-set (ctx-default) return))))))))
