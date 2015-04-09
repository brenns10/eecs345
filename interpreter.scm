;; EECS 345 Project Part 3
;; Stephen Brennan (smb196)
;; Joe Fennimore (jrf118)
;; Kaan Atesoglu (aka43)

(load "functionParser.scm")


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
(define layer-cdr
  (lambda (layer)
      (list (cdar layer) (cdadr layer))))

;; Return true if the layer is empty
(define layer-empty?
  (lambda (layer)
    (null? (car layer))))

;; Add a (var value) binding to the layer.
(define add-to-layer
  (lambda (layer var value)
    (list (cons var (car layer)) (cons value (cadr layer)))))

;; Lookup the binding for var in the state layer.
(define layer-lookup
  (lambda (layer var)
    (cond
     ((layer-empty? layer) 'not_found)
     ((equal? var (firstvar layer)) (firstval layer))
     (else (layer-lookup (layer-cdr layer) var)))))


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
(define remove-layer cdr)

;; Add a (var value) binding to the state.
(define state-add
  (lambda (state var value)
    (cons (add-to-layer (car state) var (box value)) (cdr state))))

;; Helper for the following functions
(define state-get-binding
  (lambda (state var)
    (if (null? state)
         'not_found
         (let ((val (layer-lookup (car state) var)))
           (if (eq? val 'not_found)
               (state-get-binding (cdr state) var)
               val)))))

;; The state layers only to the depth that the function name was declared.
; Last element of state is the global, outer layer.
(define trim-state
  (lambda (funcname state)
    (if (null? state)
        'func_name_not_found
        (let ((val (layer-lookup (car state) funcname)))
          (if (eq? val 'not_found)
              (trim-state (cdr state) funcname)
              state)))))

;; Lookup the binding for var in the state.
(define state-lookup
  (lambda (state var)
    (let ((val (state-get-binding state var)))
      (if (eq? val 'not_found)
          (error "Variable binding not found.")
          (unbox val)))))

;; Return whether the variable is in the state.
(define state-member?
  (lambda (state var)
    (not (eq? (state-get-binding state var) 'not_found))))

;; Update the binding for a variable in the state, preserving its layer
;; location.
(define state-update
  (lambda (state var value)
    (let ((box (state-get-binding state var)))
      (if (eq? box 'not_found)
          (error "Variable binding not found.")
          (begin
            (set-box! box value)
            state)))))


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
  (lambda (expr state return break continue)
    (if (= 3 (length expr))
        ;; A binary operator:
        ((opfunc-binary (operator expr))
         (Mvalue (leftoperand expr) state return break continue)
         (Mvalue (rightoperand expr) state return break continue))
        ;; A unary operator:
        ((opfunc-unary (operator expr))
         (Mvalue (leftoperand expr) state return break continue)))))

;; Returns the value of a statement.  This is only currently implemented for
;; assignment statements (because they're kinda expressions too).
(define Mvalue_assign
  (lambda (expr state return break continue)
      (state-lookup (Mstate_assign expr state return break continue) (cadr expr))))

;; Returns the value of a parse tree fragment which is just an atom (could be
;; either a variable or literal).
(define Mvalue_atom
  (lambda (expr state return break continue)
    (cond
      ((or (boolean? expr) (number? expr)) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((eq? 'undefined (state-lookup state expr))
       (error "Use of undefined variable."))
      (else (state-lookup state expr)))))

;; Return the value of any parse tree fragment!
(define Mvalue
  (lambda (expr state return break continue)
    (cond
     ((list? expr) (cond
                    ((eq? '= (car expr)) (Mvalue_assign expr state return break continue))
                    (else (Mvalue_expression expr state return break continue))))
     (else (Mvalue_atom expr state return break continue)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mboolean functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns #t or #f based on the boolean evaluation of the expression Right now,
;; Mvalue already performs the function that we want for Mboolean, and so for
;; the sake of abstraction we're keeping a separate Mboolean function, but for
;; the sake of non-redundant code, we're not repeating the code in Mvalue.
(define Mboolean
  (lambda (expr state return break continue)
    (Mvalue expr state return break continue)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mstate functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the state after executing an if statement.
(define Mstate_if
  (lambda (stmt state return break continue)
    (if (= 3 (length stmt))
          ; IF statement
          (if (Mboolean (list-ref stmt 1) state return break continue)
              (Mstate (list-ref stmt 2) state return break continue)
              state)
          ; ELSE IF
          (if (Mboolean (list-ref stmt 1) state return break continue)
              (Mstate (list-ref stmt 2) state return break continue)
              (Mstate (list-ref stmt 3) state return break continue)))))

;; Return the state after executing a declaration.
(define Mstate_declare
  (lambda (stmt state return break continue)
    (cond
     ((state-member? state (cadr stmt))
      (error "Redeclaring variable."))
     ((= 3 (length stmt))
      ;; This is declaration AND assignment.
      (state-add (Mstate (caddr stmt) state return break continue)
                 (cadr stmt)
                 (Mvalue (caddr stmt) state return break continue)))
     ;; This is just declaration.
     (else (state-add state (cadr stmt) 'undefined)))))

(define Mstate_assign
  (lambda (stmt state return break continue)
    (if (state-member? state (cadr stmt))
          (state-update state (cadr stmt) (Mvalue (caddr stmt) state return break continue))
          (error "Using variable before declared."))))

(define Mstate_return
  (lambda (stmt state return break continue)
    (return  (Mvalue (cadr stmt) state return break continue))))

;; Helper method to handle the fact that return statements should return
;; the atoms 'true or 'false rather than #t and #f
(define return_val
  (lambda (stmt)
    (cond
      ((eq? stmt #t) 'true)
      ((eq? stmt #f) 'false)
      (else stmt))))

;; Execute a list of statements.  This doesn't add a layer, it just executes the
;; statements in order.
(define Mstate_stmtlist
  (lambda (block state return break continue)
    (if (null? block)
        state
        (Mstate_stmtlist (cdr block)
                         (Mstate (car block) state return
                                 break continue)
                         return break continue))))

;; Execute a block of statements.  This is different from a statement list in
;; that it adds a layer to the state, then removes it.
(define Mstate_block
  (lambda (block state return break continue)
    (remove-layer (Mstate_stmtlist (cdr block) (add-layer state)
                                   ;; Modify the break and continue
                                   ;; continuations so that they remove the
                                   ;; correct number of layers when they fire.
                                   (lambda (s) (return (remove-layer s)))
                                   (lambda (s) (break (remove-layer s)))
                                   (lambda (s) (continue (remove-layer s)))))))

;; Executes a while statement.
(define Mstate_while
  (lambda (stmt state return break continue)
    ;; Create the new break continuation.
    (call/cc
     (lambda (break_new)
       (letrec
           ((loop (lambda (condition body state)
                    (if (Mboolean condition state return break_new continue)
                        ;; If the loop condition is true, tail recursively loop
                        ;; again.
                        (loop condition body
                              ;; Create a continue continuation
                              (call/cc (lambda (continue_new)
                                         (Mstate body state
                                                 return break_new
                                                 continue_new))))
                        state))))
         ;; Execute the inner loop:
         (loop (cadr stmt) (caddr stmt) state))))))

;; Binds the name of this function to the closure
 ; The given funcdecl has the form:
 ; function a(x, y) { return x + y } => (function a (x y) ((return (+ x y)))
(define Mstate_funcdecl
  (lambda (funcdecl state return break continue)
    (let (fname (cadr funcdecl))
    (if (state-member? state fname)
      (error "Redefining function.")
        (state-add fname
                 ((caddr funcdecl) ; Parameter list
                  (cadddr funcdecl) ; Body
                  (lambda (fname state) ; Function to create the appropriate environment
                    trim-state (fname state))))))))
    

;; Return the state after executing any parse tree fragment.
(define Mstate
  (lambda (stmt state return break continue)
    (cond
     ((null? stmt) state)
     ((list? stmt) (cond
                    ((list? (car stmt)) (Mstate_stmtlist stmt state return break continue))
                    ((eq? 'begin (car stmt)) (Mstate_block stmt state return break continue))
                    ((eq? 'var (car stmt)) (Mstate_declare stmt state return
                                                           break continue))
                    ((eq? '= (car stmt)) (Mstate_assign stmt state return
                                                        break continue))
                    ((eq? 'if (car stmt)) (Mstate_if stmt state return break continue))
                    ((eq? 'return (car stmt)) (Mstate_return stmt state return break continue))
                    ((eq? 'break (car stmt)) (break state))
                    ((eq? 'continue (car stmt)) (continue state))
                    ((eq? 'while (car stmt)) (Mstate_while stmt state return break continue))
                    ((eq? 'function (car stmt)) (Mstate_func stmt state return break continue))
                    (else state)))
     (else state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overall interpreter functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interpret from the given filename, and return its value.
(define interpret
  (lambda (filename)
    (return_val (call/cc
                 (lambda (return)
                   (Mstate (parser filename) (state-new) return #f #f))))))
