;; EECS 345 Project Part 1
;; Stephen Brennan (smb196)
;; Joe Fennimore (jrf118)
;; Kaan Atesoglu (aka43)

(load "simpleParser.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns true if the argument is an atom.
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

;; There appears not to be a native != function, so we're going to create one,
;; specifically for numbers
(define !=
  (lambda (x y)
    (not (= x y))))

;; Perform a left fold.  Takes a function which takes two arguments, an initial
;; value to supply to the function, and a list of arguments.  Applies the
;; function to the initial value (as the first argument) and the first element
;; of the list (as the second argument).  The return value is then used as the
;; initial value for the next element of the list.  EG:
;; (fold-left + 0 '(1 2 3 4)) => 10
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

;; Return a new, empty state.
(define state-new
  (lambda () '(() ())))

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
(define state-add
  (lambda (state var value)
    (list (cons var (car state)) (cons value (cadr state)))))

;; Remove the first binding for var from the state.
(define state-remove
  (lambda (state var)
    (cond
     ;; If the state is empty, the binding is removed.
     ((snull? state) state)
     ;; If the first variable in the state is the variable, return the rest of
     ;; the state.
     ((eq? var (firstvar state)) (scdr state))
     ;; Otherwise, put the current variable and value onto the state with the
     (else (state-add (state-remove (scdr state) var)
                      (firstvar state) (firstval state))))))

;; Lookup the binding for var in state.
(define state-lookup
  (lambda (state var)
    (cond
     ((snull? state) (error "Variable binding not found"))
     ((equal? var (firstvar state)) (firstval state))
     (else (state-lookup (scdr state) var)))))

;; Return whether the variable is in the state.
(define state-member
  (lambda (state var)
    (cond
     ((snull? state) #f)
     ((equal? var (firstvar state)) #t)
     (else (state-member (scdr state) var)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mvalue functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These define the operator and operands of prefix form expressions.
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

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

;; This function takes a unary operator and returns a Scheme function
;; implementing it.
(define opfunc-unary
  (lambda (op)
    (cond
      ((eq? op '!) not)
      ((eq? op '-) (lambda (x) (- 0 x)))
      (else (error "Unrecognized unary operator.")))))

;; Returns the value of an arithmetic expression.
(define Mvalue_expression
  (lambda (expression state)
    (if (= 3 (length expression))
        ;; A binary operator:
        ((opfunc-binary (operator expression))
         (Mvalue (leftoperand expression) state)
         (Mvalue (rightoperand expression)
                 (Mstate (leftoperand expression) state)))
        ;; A unary operator:
        ((opfunc-unary (operator expression))
         (Mvalue (leftoperand expression) state)))))

;; Returns the value of a statement.  This is only currently implemented for
;; return statements.  It will need to be implemented for assignment and if.
;; Change to cond for that.
(define Mvalue_statement
  (lambda (expression state)
    (cond
      ; Mvalue for assign stmts
      ((eq? (car expression) '=) (Mvalue (caddr expression) state))
      (else #f))))

;; Returns the value of a parse tree fragment which is a list (could be either
;; an expression or statement).
(define Mvalue_list
  (lambda (expression state)
    (cond
     ((member (car expression) statement) (Mvalue_statement expression state))
     (else (Mvalue_expression expression state)))))

;; Returns the value of a parse tree fragment which is just an atom (could be
;; either a variable or literal).
(define Mvalue_atom
  (lambda (expression state)
    (cond
      ((or (boolean? expression) (number? expression)) expression)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((eq? 'undefined (state-lookup state expression))
       (error "Use of undefined variable."))
      (else (state-lookup state expression)))))

;; Return the value of any parse tree fragment!
(define Mvalue
  (lambda (expression state)
    (cond
     ((atom? expression) (Mvalue_atom expression state))
     (else (Mvalue_list expression state)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mboolean functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns #t or #f based on the boolean evaluation of the expression Right now,
;; Mvalue already performs the function that we want for Mboolean, and so for
;; the sake of abstraction we're keeping a separate Mboolean function, but for
;; the sake of non-redundant code, we're not repeating the code in Mvalue.
(define Mboolean
  (lambda (expression state)
    (Mvalue expression state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mstate functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the state after executing an if statement.
(define Mstate_if
  (lambda (expression state)
    (if (= 3 (length expression))
          ; IF statement
          (if (Mboolean (list-ref expression 1) state)
              (Mstate (list-ref expression 2) (Mstate (list-ref expression 1)
                                                      state))
              (Mstate (list-ref expression 1) state))
          ; ELSE IF
          (if (Mboolean (list-ref expression 1) state)
              (Mstate (list-ref expression 2) (Mstate (list-ref expression 1)
                                                      state))
              (Mstate (list-ref expression 3) (Mstate (list-ref expression 1)
                                                      state))))))

;; Return the state after executing a declaration.
(define Mstate_declare
  (lambda (expression state)
    (cond
     ((state-member state (cadr expression))
      (error "Redeclaring variable."))
     ((= 3 (length expression))
      ;; This is declaration AND assignment.
      (state-add (state-remove (Mstate (caddr expression) state)
                               (cadr expression))
                 (cadr expression)
                 (Mvalue (caddr expression) state)))
        ;; This is just declaration.
     (else (state-add state (cadr expression) 'undefined)))))

;; Return the state after executing a statement.
(define Mstate_statement
  (lambda (expression state)
    (cond
     ((eq? 'var (car expression)) (Mstate_declare expression state))
     ((eq? '= (car expression))
      (if (state-member state (cadr expression))
          (state-add (state-remove (Mstate (caddr expression) state)
                                   (cadr expression))
                     (cadr expression) (Mvalue (caddr expression) state))
          (error "Using variable before declared.")))
     ((eq? 'return (car expression))
      (state-add (state-remove state "*return value*")
                 "*return value*"
                 (return_val (Mvalue (cadr expression) state))))
     ((eq? 'if (car expression)) (Mstate_if expression state)))))

;; Helper method to handle the fact that return statements should return
;; the atoms 'true or 'false rather than #t and #f
(define return_val
  (lambda (expression)
    (cond
      ((eq? expression #t) 'true)
      ((eq? expression #f) 'false)
      (else expression))))

;; Since expressions may have assignments within them, you need to call Mstate
;; on each of the parts of the expression in order to get the state from them.
(define Mstate_expression
  (lambda (expression state)
    (fold-left (lambda (state exp) (Mstate exp state))
               state (cdr expression))))

;; Return the state after executing a parse tree fragment which is a list (could
;; be either an expression or statement).
(define Mstate_list
  (lambda (expression state)
    (if (member (car expression) statement)
        (Mstate_statement expression state)
        (Mstate_expression expression state))))

;; Return the state after executing any parse tree fragment.
(define Mstate
  (lambda (expression state)
    (if (list? expression)
        (Mstate_list expression state)
        state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overall interpreter functions
;;
;; These implement the actual high-level interpreter action.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interpret from the given filename, and return its value.
(define interpret
  (lambda (filename)
    (interpret_parsetree (parser filename) (state-new))))

;; Given a parsed tree, interpret with the given state until the whole tree has
;; been interpreted.
(define interpret_parsetree
  (lambda (tree state)
    (if (null? tree)
        (if (state-member state "*return value*")
            (state-lookup state "*return value*")
            'no_return_value)
        (interpret_parsetree (cdr tree) (Mstate (car tree) state)))))
