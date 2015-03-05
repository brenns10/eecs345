;; EECS 345 Project Part 1
;; Stephen Brennan (smb196)
;; Joe Fennimore (jrf118)
;; Kaan Atesoglu (aka43)

(load "simpleParser.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; Layer functions:  '((var_name1 var_name2) (var_value1 var_value2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A new layer.
(define layer-new
  (lambda () '(() ())))

;; The first variable in the layer.
(define firstvar caar)

;; The value of the first variable in the layer.
(define firstval caadr)

;; Return the layer with all but the first binding present.
(define scdr
  (lambda (layer)
      (list (cdar layer) (cdadr layer))))

;; Return true if the layer is empty
(define snull?
  (lambda (layer)
    (null? (car layer))))

;; Add a (var value) binding to the layer.
(define add-to-layer
  (lambda (layer var value)
    (list (cons var (car layer)) (cons value (cadr layer)))))

;; Remove the first binding for var from the layer.
(define remove-from-layer
  (lambda (layer var)
    (cond
     ;; If the layer is empty, the binding is removed.
     ((snull? layer) layer)
     ;; If the first variable in the layer is the variable, return the rest of
     ;; the layer.
     ((eq? var (firstvar layer)) (scdr layer))
     ;; Otherwise, put the current variable and value onto the layer with the
     (else (add-to-layer (remove-from-layer (scdr layer) var)
                         (firstvar layer) (firstval layer))))))

;; Lookup the binding for var in the state layer.
(define layer-lookup
  (lambda (layer var)
    (cond
     ((snull? layer) 'not_found)
     ((equal? var (firstvar layer)) (firstval layer))
     (else (layer-lookup (scdr layer) var)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State functions (states are lists of layers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return a new, empty state.
(define state-new
  (lambda () '((() ()))))

;; Add a layer to the state.
(define add-layer
  (lambda (state)
    (cons (layer-new) state)))

;; Remove a layer from the state.
(define remove-layer
  (lambda (state)
    (cdr state)))

;; Add a (var value) binding to the state.
(define state-add
  (lambda (state var value)
    (cons (add-to-layer (car state) var value) (cdr state))))

;; Remove the first binding for var from the state.  (TODO: remove this before
;; turning in)
(define state-remove
  (lambda (state var)
    (cond
     ((null? state) state)
     ((not (eq? (car state) (remove-from-layer (car state) var)))
      (cons (remove-from-layer (car state) var) (cdr state)))
     (else (cons (car state) (state-remove (cdr state) var))))))

;; Lookup the binding for var in the state.
(define state-lookup
  (lambda (state var)
    (cond
     ((null? state) (error "Variable binding not found."))
     ((eq? (layer-lookup (car state) var) 'not_found)
      (state-lookup (cdr state) var))
     (else (layer-lookup (car state) var))))) ; TODO: can improve with let

;; Return whether the variable is in the state.
(define state-member
  (lambda (state var)
    (cond
     ((null? state) #f)
     ((eq? (layer-lookup (car state) var) 'not_found)
      (state-member (cdr state) var))
     (else #t))))

;; Update the binding for a variable in the state, preserving its layer
;; location.
(define state-update
  (lambda (state var value)
    (cond
     ;; No more layers:
     ((null? state) (error "Variable binding not found."))
     ;; The layer doesn't contain the variable:
     ((eq? (layer-lookup (car state) var) 'not_found)
      (cons (car state) (state-update (cdr state) var value)))
     ;; The layer does contain the variable:
     (else (cons (add-to-layer (remove-from-layer (car state) var) var value)
                 (cdr state))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mvalue functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These define the operator and operands of prefix form expressions.
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

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
  (lambda (expr state)
    (if (= 3 (length expr))
        ;; A binary operator:
        ((opfunc-binary (operator expr))
         (Mvalue (leftoperand expr) state)
         (Mvalue (rightoperand expr)
                 (Mstate (leftoperand expr) state)))
        ;; A unary operator:
        ((opfunc-unary (operator expr))
         (Mvalue (leftoperand expr) state)))))

;; Returns the value of a statement.  This is only currently implemented for
;; assignment statements (because they're kinda expressions too).
(define Mvalue_assign
  (lambda (expr state)
      (Mvalue (caddr expr) state)))

;; Returns the value of a parse tree fragment which is just an atom (could be
;; either a variable or literal).
(define Mvalue_atom
  (lambda (expr state)
    (cond
      ((or (boolean? expr) (number? expr)) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((eq? 'undefined (state-lookup state expr))
       (error "Use of undefined variable."))
      (else (state-lookup state expr)))))

;; Return the value of any parse tree fragment!
(define Mvalue
  (lambda (expr state)
    (cond
     ((list? expr) (cond
                    ((eq? '= (car expr)) (Mvalue_assign expr state))
                    (else (Mvalue_expression expr state))))
     (else (Mvalue_atom expr state)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mboolean functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns #t or #f based on the boolean evaluation of the expression Right now,
;; Mvalue already performs the function that we want for Mboolean, and so for
;; the sake of abstraction we're keeping a separate Mboolean function, but for
;; the sake of non-redundant code, we're not repeating the code in Mvalue.
(define Mboolean
  (lambda (expr state)
    (Mvalue expr state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mstate functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the state after executing an if statement.
(define Mstate_if
  (lambda (stmt state)
    (if (= 3 (length stmt))
          ; IF statement
          (if (Mboolean (list-ref stmt 1) state)
              (Mstate (list-ref stmt 2) (Mstate (list-ref stmt 1)
                                                      state))
              (Mstate (list-ref stmt 1) state))
          ; ELSE IF
          (if (Mboolean (list-ref stmt 1) state)
              (Mstate (list-ref stmt 2) (Mstate (list-ref stmt 1)
                                                      state))
              (Mstate (list-ref stmt 3) (Mstate (list-ref stmt 1)
                                                      state))))))

;; Return the state after executing a declaration.
(define Mstate_declare
  (lambda (stmt state)
    (cond
     ((state-member state (cadr stmt))
      (error "Redeclaring variable."))
     ((= 3 (length stmt))
      ;; This is declaration AND assignment.
      (state-add (Mstate (caddr stmt) state)
                 (cadr stmt)
                 (Mvalue (caddr stmt) state)))
     ;; This is just declaration.
     (else (state-add state (cadr stmt) 'undefined)))))

(define Mstate_assign
  (lambda (stmt state)
    (if (state-member state (cadr stmt))
          (state-update (Mstate (caddr stmt) state)
                        (cadr stmt) (Mvalue (caddr stmt) state))
          (error "Using variable before declared."))))

(define Mstate_return
  (lambda (stmt state)
    (state-add (state-remove state "*return value*")
               "*return value*"
               (return_val (Mvalue (cadr stmt) state)))))

;; Helper method to handle the fact that return statements should return
;; the atoms 'true or 'false rather than #t and #f
(define return_val
  (lambda (stmt)
    (cond
      ((eq? stmt #t) 'true)
      ((eq? stmt #f) 'false)
      (else stmt))))

;; Since expressions may have assignments within them, you need to call Mstate
;; on each of the parts of the expression in order to get the state from them.
(define Mstate_expression
  (lambda (stmt state)
    (fold-left (lambda (state stmt2) (Mstate stmt2 state))
               state (cdr stmt))))

;; Return the state after executing any parse tree fragment.
(define Mstate
  (lambda (stmt state)
    (cond
     ((null? stmt) state)
     ((list? stmt) (cond
                    ((list? (car stmt)) (Mstate (cdr stmt) (Mstate (car stmt)
                                                                   state)))
                    ((eq? 'var (car stmt)) (Mstate_declare stmt state))
                    ((eq? '= (car stmt)) (Mstate_assign stmt state))
                    ((eq? 'if (car stmt)) (Mstate_if stmt state))
                    ((eq? 'return (car stmt)) (Mstate_return stmt state))
                    (else (Mstate_expression stmt state))))
     (else state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overall interpreter functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interpret from the given filename, and return its value.
(define interpret
  (lambda (filename)
    (state-lookup (Mstate (parser filename) (state-new)) "*return value*")))
