(load "simpleParser.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interacting with the state.
;;
;; Current implementation of state is: '((var names ...) (var values)), since
;; Dr. Connamacher said that would be easier for the future.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A new, empty state.
(define state_new '(() ()))

;; The first variable in the state.
(define firstvar caar)

;; The value of the first variable in the state.
(define firstval caadr)

;; Return the state with all but the first binding present.
(define scdr
  (lambda (state)
      (list (cdar state) (cdadr state))))

;; Return true if the state is empty
(define snull?
  (lambda (state)
    (null? (car state))))

;; Add a (var value) binding to the state.
(define state_add
  (lambda (state var value)
    (list (cons var (car state)) (cons value (cadr state)))))

;; Remove the first binding for var from the state.
(define state_remove
  (lambda (state var)
    (cond
     ;; If the state is empty, the binding is removed.
     ((snull? state) state)
     ;; If the first variable in the state is the variable, return the rest of
     ;; the state.
     ((eq? var (firstvar state)) (scdr state))
     ;; Otherwise, put the current variable and value onto the state with the
     (else (state_add (state_remove (scdr state) var)
                      (firstvar state) (firstval state))))))

;; Lookup the binding for var in state.
(define state_lookup
  (lambda (state var)
    (cond
     ((snull? state) (error "Variable binding not found"))
     ((eq? var (firstvar state)) (firstval state))
     (else (state_lookup (scdr state) var)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overall interpreter functions
;;
;; These implement the actual high-level interpreter action.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interpret from the given filename, and return its value.
(define interpret
  (lambda (filename)
    (interpret_parsetree (parser filename) state_new)))

;; Given a parsed tree, interpret with the given state until the whole tree has
;; been interpreted.
(define interpret_parsetree
  (lambda (tree state)
    (if (null? (cdr tree))
        (Mvalue (car tree) state)
        (interpret_parsetree (cdr tree) (Mstate (car tree) state)))))
