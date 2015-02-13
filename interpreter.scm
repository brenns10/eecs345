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

(define fold-left
  (lambda (function initial list)
    (if (null? list)
        initial
        (fold-left function (function initial (car list)) (cdr list)))))

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
     ((equal? var (firstvar state)) (firstval state))
     (else (state_lookup (scdr state) var)))))

;; Return whether the variable is in the state.
(define state_member
  (lambda (state var)
    (cond
     ((snull? state) #f)
     ((equal? var (firstvar state)) #t)
     (else (state_member (scdr state) var)))))


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
(define opfunc-binary
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

      (else (error "Unrecognized binary operator.")))))

(define opfunc-unary
  (lambda (op)
    (cond
      ((eq? op '!) not)
      ((eq? op '-) (lambda (x) (- 0 x)))
      (else (error "Unrecognized unary operator.")))))

;; Returns the value of an arithmetic expression.
(define Mvalue_expression
  (lambda (expression state form)
    ((lambda (operator leftoperand rightoperand)
       (if (= 3 (length expression))
           ((opfunc-binary (operator expression))
            (Mvalue (leftoperand expression) state form)
            (Mvalue (rightoperand expression) (Mstate (leftoperand expression)
                                                      state form) form))
           ((opfunc-unary (operator expression))
            (Mvalue (leftoperand expression) state form))))
     (operator form) (leftoperand form) (rightoperand form))))

;; Returns the value of a statement.  This is only currently implemented for
;; return statements.  It will need to be implemented for assignment and if.
;; Change to cond for that.
(define Mvalue_statement
  (lambda (expression state form)
    (cond
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

;; Return the state after executing an if statement.
(define Mstate_if
  (lambda (expression state form)
    (if (= 3 (length expression))
          ; IF statement
          (if (Mboolean (list-ref expression 1) state form)
              (Mstate (list-ref expression 2) (Mstate (list-ref expression 1)
                                                      state form) form)
              (Mstate (list-ref expression 1) state form))
          ; ELSE IF
          (if (Mboolean (list-ref expression 1) state form)
              (Mstate (list-ref expression 2) (Mstate (list-ref expression 1)
                                                      state form) form)
              (Mstate (list-ref expression 3) (Mstate (list-ref expression 1)
                                                      state form) form)))))

;; Return the state after executing a declaration.
(define Mstate_declare
  (lambda (expression state form)
    (cond
     ((state_member state (cadr expression))
      (error "Redeclaring variable."))
     ((= 3 (length expression))
      ;; This is declaration AND assignment-
      (state_add (state_remove (Mstate (caddr expression) state form)
                               (cadr expression))
                 (cadr expression)
                 (Mvalue (caddr expression) state form)))
        ;; This is just declaration.
     (else (state_add state (cadr expression) 'undefined)))))

;; Return the state after executing a statement.
(define Mstate_statement
  (lambda (expression state form)
    (cond
     ((eq? 'var (car expression)) (Mstate_declare expression state form))
     ((eq? '= (car expression))
      (if (state_member state (cadr expression))
          (state_add (state_remove (Mstate (caddr expression) state form)
                                   (cadr expression))
                     (cadr expression) (Mvalue (caddr expression) state form))
          (error "Using variable before declared.")))
     ((eq? 'return (car expression))
      (state_add (state_remove state "*return value*")
                 "*return value*" (Mvalue (cadr expression) state form)))
     ((eq? 'if (car expression)) (Mstate_if expression state form)))))

;; Since expressions may have assignments within them, you need to call Mstate
;; on each of the parts of the expression in order to get the state from them.
(define Mstate_expression
  (lambda (expression state form)
    (fold-left (lambda (state exp) (Mstate exp state form))
               state (cdr expression))))

;; Return the state after executing a parse tree fragment which is a list (could
;; be either an expression or statement).
(define Mstate_list
  (lambda (expression state form)
    (if (member (car expression) statement)
        (Mstate_statement expression state form)
        (Mstate_expression expression state form))))

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
    (if (null? tree)
        (if (state_member state "*return value*")
            (state_lookup state "*return value*")
            'no_return_value)
        (interpret_parsetree (cdr tree) (Mstate (car tree) state prefix)))))
