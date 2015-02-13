(load "simpleParser.scm")

;; Returns true if the argument is an atom.
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

;; There appears not to be a native != function, so we're going to create one,
;; specifically for numbers
(define !=
  (lambda (x y)
    (not (= x y))))

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
;; Mvalue functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These functions take an expression form (infix, prefix, or postfix) and
;; return the function that will give the respective parts of the expression.
(define operator
  (lambda (form)
    (car form)))
(define leftoperand
  (lambda (form)
    (cadr form)))
(define rightoperand
  (lambda (form)
    (caddr form)))

;; These functions define the expression forms.
(define infix (list cadr car caddr))
(define prefix (list car cadr caddr))
(define postfix (list caddr car cadr))

;; This list defines what "operators" are statements.
(define statement '(var = if return))

;; This function takes an operator atom and returns the Scheme function that
;; corresponds to it.
(define opfunc
  (lambda (op)
    (cond
      ((eq? op '+) +)
      ((eq? op '-) -)
      ((eq? op '/) quotient)
      ((eq? op '*) *)
      ((eq? op '%) remainder)

      ; Boolean functions
      ((eq? op '&&) (lambda (x y) (and x y))) ; Can't use 'and' as a function
                                              ; name- it's a keyword
      ((eq? op '||) (lambda (x y) (or x y)))
      ((eq? op '<) <)
      ((eq? op '>) >)
      ((eq? op '<=) <=)
      ((eq? op '>=) >=)
      ((eq? op '==) =)
      ((eq? op '!=) !=)

      (else (error "Unrecognized operator.")))))

;; Returns the value of an arithmetic expression.
(define Mvalue_expression
  (lambda (expression state form)
    ((lambda (operator leftoperand rightoperand)
       ((opfunc (operator expression))
        (Mvalue (leftoperand expression) state form)
        (Mvalue (rightoperand expression) state form)))
     (operator form) (leftoperand form) (rightoperand form))))

;; Returns the value of a statement.  This is only currently implemented for
;; return statements.  It will need to be implemented for assignment and if.
;; Change to cond for that.
(define Mvalue_statement
  (lambda (expression state form)
    (cond
      ((eq? (car expression) 'return)
        (Mvalue (cadr expression) state form))
      ; Mvalue for assign stmts
      ((eq? (car expression) '=) (Mvalue (caddr expression) state form))
      (else #f))))

;; Returns the value of a parse tree fragment which is a list (could be either
;; an expression or statement).
(define Mvalue_list
  (lambda (expression state form)
    ((lambda (operator)
       (cond
        ((member (operator expression) statement)
         (Mvalue_statement expression state form))
        (else (Mvalue_expression expression state form))))
     (operator form))))

;; Returns the value of a parse tree fragment which is just an atom (could be
;; either a variable or literal).
(define Mvalue_atom
  (lambda (expression state form)
    (cond
      ((or (boolean? expression) (number? expression)) expression)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((eq? 'undefined (state_lookup state expression))
       (error "Use of undefined variable."))
      (else (state_lookup state expression)))))

;; Return the value of any parse tree fragment!
(define Mvalue
  (lambda (expression state form)
    (cond
     ((atom? expression) (Mvalue_atom expression state form))
     (else (Mvalue_list expression state form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mboolean functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns #t or #f based on the boolean evaluation of the expression Right now,
;; Mvalue already performs the function that we want for Mboolean, and so for
;; the sake of abstraction we're keeping a separate Mboolean function, but for
;; the sake of non-redundant code, we're not repeating the code in Mvalue.
(define Mboolean
  (lambda (expression state form)
    (Mvalue expression state form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mstate functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the state after executing a statement.
(define Mstate_statement
  (lambda (expression state form)
    (cond
     ((eq? 'var (car expression))
      (state_add state (cadr expression) 'undefined))
     ((eq? '= (car expression))
      (state_add (state_remove state (cadr expression))
                 (cadr expression) (Mvalue (caddr expression) state form)))
     ((eq? 'if (car expression))
      (if (null? (list-ref expression 4)) 
          ; IF statement
          (if (Mboolean (list-ref expression 2) state form) 
              (Mstate (list-ref expression 3) (Mstate (list-ref expression 2) state form) form) 
              (Mstate (list-ref expression 2) state form)) 
          ; ELSE IF
          (if (Mboolean (list-ref expression 2) state form) 
              (Mstate (list-ref expression 3) (Mstate (list-ref expression 2) state form) form) 
              (Mstate (list-ref expression 4) (Mstate (list-ref expression 2) state form) form)) 
          (else state))))))

;; Return the state after executing a parse tree fragment which is a list (could
;; be either an expression or statement).
(define Mstate_list
  (lambda (expression state form)
    (if (member (car expression) statement)
        (Mstate_statement expression state form)
        state)))

;; Return the state after executing any parse tree fragment.
(define Mstate
  (lambda (expression state form)
    (if (list? expression)
        (Mstate_list expression state form)
        state)))


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
        (Mvalue (car tree) state prefix)
        (interpret_parsetree (cdr tree) (Mstate (car tree) state prefix)))))
