;; EECS 345 Project Part 3
;; Stephen Brennan (smb196)
;; Joe Fennimore (jrf118)
;; Kaan Atesoglu (aka43)

(load "classParser.scm")


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

;; Use a YC + CPS, cause why not?
(define index-of
  (lambda (list item)
    (((lambda (f) (f f))
      (lambda (f)
        (lambda (list item return)
          (cond
           ((null? list) -1) ;; No CPS return
           ((eq? (car list) item) (return 0))
           (else ((f f) (cdr list) item (lambda (v) (return (+ 1 v)))))))))
     list item (lambda (v) v))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layer/Environment functions:  '((var_name1 var_name2) (var_value1 var_value2))
;; - There are two interfaces defined here - layer and environment.
;; - Layer is blind to the existence of boxes, and so it's very useful to the
;;   state, which handles all the boxing and unboxing for you.
;; - Environment knows about boxes, and it's good for use in class/instances.
;; - There's no difference in the data structure, or how they search for stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A new layer.
(define layer-new
  (lambda () '(() ())))
(define env-new layer-new)

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
(define env-empty? layer-empty?)

;; Add a (var value) binding to the layer.
(define add-to-layer
  (lambda (layer var value)
    (list (cons var (car layer)) (append (cadr layer) (list value)))))
(define env-add
  (lambda (env var value) (add-to-layer (env var (box value)))))

;; Lookup the binding for var in the state layer.
(define layer-lookup
  (lambda (layer var)
    (let ((idx (index-of (car layer) var)))
      (if (= -1 idx)
          'not_found
          (list-ref (cadr layer) (- (length (cadr layer)) idx 1))))))
(define env-lookup
  (lambda (env var)
    (let ((val (layer-lookup env var)))
      (if (eq? value 'not_found)
          'not_found
          (unbox val)))))

(define env-update
  (lambda (env var new)
    (let ((box (layer-lookup env var)))
      (if (eq? box 'not_found)
          (error "Variable binding not found.")
          (set-box! box new)))))

(define layer-member?
  (lambda (layer var)
    (not (= -1 (index-of (car layer) var)))))
(define env-member? layer-member?)

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
    (if (layer-member? (car state) var)
        (error "Redeclaring variable.")
        (cons (add-to-layer (car state) var (box value)) (cdr state)))))

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
        (error "Function name not found.")
        (let ((val (layer-lookup (car state) funcname)))
          (if (eq? val 'not_found)
              (trim-state funcname (cdr state))
              state)))))

(define state-lookup-box
  (lambda (state var)
    (let ((val (state-get-binding state var)))
      (if (eq? val 'not_found)
          (error "Variable binding not found.")
          val))))

;; Lookup the binding for var in the state.
(define state-lookup
  (lambda (state var) (unbox (state-lookup-box state var))))


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
;; Class functions: A class is a list containing the following (in order):
;; - Parent class, or 'null.
;; - Class field environment.
;; - Method environment.
;; - Instance field names.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define class-new
  (lambda (parent)
    (list parent (env-new) (env-new) '())))

(define class-parent car)
(define class-fields cadr)
(define class-methods caddr)
(define class-instance-names cadddr)

(define class-fields-set
  (lambda (cls fields)
    (list (class-parent cls)
          fields
          (class-methods cls)
          (class-instance-names cls))))

(define class-methods-set
  (lambda (cls methods)
    (list (class-parent cls)
          (class-fields cls)
          methods
          (class-instance-names cls))))

(define class-instance-names-set
  (lambda (cls instance-names)
    (list (class-parent cls)
          (class-fields cls)
          (class-methods cls)
          instance-names)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instance functions: An instance is a list containing:
;; - The class
;; - The instance field values.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define inst-new
  (lambda (cls)
    (list cls '())))

(define inst-cls car)
(define inst-values cadr)

(define inst-values-set
  (lambda (inst values)
    (list (inst-cls) values)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context Functions (a context contains all the damn continuations)
;; => The context is a list: '(return break continue)
;; => I made it like this so we don't have to change function signatures every
;;    damn time!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; New continuation - contains all caller-defined values
(define ctx-new list)

;; The functions for accessing items in the continuation.
(define ctx-return car)
(define ctx-break cadr)
(define ctx-continue caddr)

;; The functions for modifying items in the continuation.
(define ctx-return-set
  (lambda (ctx return)
    (cons return (cdr ctx))))

(define ctx-break-set
  (lambda (ctx break)
    (list (ctx-return ctx) break (ctx-break ctx))))

(define ctx-continue-set
  (lambda (ctx continue)
    (list (ctx-return ctx) (ctx-break ctx) continue)))

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
  (lambda (expr state ctx)
    (if (= 3 (length expr))
        ;; A binary operator:
        ((opfunc-binary (operator expr))
         (Mvalue (leftoperand expr) state ctx)
         (Mvalue (rightoperand expr) state ctx))
        ;; A unary operator:
        ((opfunc-unary (operator expr))
         (Mvalue (leftoperand expr) state ctx)))))

;; Returns the value of a statement.  This is only currently implemented for
;; assignment statements (because they're kinda expressions too).
(define Mvalue_assign
  (lambda (expr state ctx)
      (state-lookup (Mstate_assign expr state ctx) (cadr expr))))

;; Returns the value of a parse tree fragment which is just an atom (could be
;; either a variable or literal).
(define Mvalue_atom
  (lambda (expr state ctx)
    (cond
      ((or (boolean? expr) (number? expr)) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((eq? 'undefined (state-lookup state expr))
       (error "Use of undefined variable."))
      (else (state-lookup state expr)))))

(define new-layer-from-arglist
  (lambda (formal actual state ctx)
    (cond
     ((and (null? formal) (null? actual)) (layer-new))
     ((or (null? formal) (null? actual)) (error "Incorrect number of args."))
     ((eq? '& (car formal)) (add-to-layer (new-layer-from-arglist (cddr formal) (cdr actual)
                                                                  state ctx)
                                          (cadr formal)
                                          (state-lookup-box state (car actual))))
     (else (add-to-layer (new-layer-from-arglist (cdr formal) (cdr actual)
                                                 state ctx)
                         (car formal) (box (Mvalue (car actual) state ctx)))))))

(define Mvalue_funccall
  (lambda (funccall state ctx)
    (let* ((closure  (state-lookup state (cadr funccall)))
           (outerenv ((caddr closure) state))
           (newstate (cons (new-layer-from-arglist (car closure) (cddr funccall)
                                                   state ctx)
                           outerenv))
           (err (lambda (v) (error "Can't break or continue here."))))
      (call/cc
       (lambda (return)
         (Mstate_stmtlist (cadr closure) newstate (ctx-new return err err)))))))

;; Return the value of any parse tree fragment!
(define Mvalue
  (lambda (expr state ctx)
    (cond
     ((list? expr) (cond
                    ((eq? '= (car expr)) (Mvalue_assign expr state ctx))
                    ((eq? 'funcall (car expr)) (Mvalue_funccall expr state ctx))
                    (else (Mvalue_expression expr state ctx))))
     (else (Mvalue_atom expr state ctx)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mboolean functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns #t or #f based on the boolean evaluation of the expression Right now,
;; Mvalue already performs the function that we want for Mboolean, and so for
;; the sake of abstraction we're keeping a separate Mboolean function, but for
;; the sake of non-redundant code, we're not repeating the code in Mvalue.
(define Mboolean
  (lambda (expr state ctx)
    (Mvalue expr state ctx)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mstate functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the state after executing an if statement.
(define Mstate_if
  (lambda (stmt state ctx)
    (if (= 3 (length stmt))
          ; IF statement
          (if (Mboolean (list-ref stmt 1) state ctx)
              (Mstate (list-ref stmt 2) state ctx)
              state)
          ; ELSE IF
          (if (Mboolean (list-ref stmt 1) state ctx)
              (Mstate (list-ref stmt 2) state ctx)
              (Mstate (list-ref stmt 3) state ctx)))))

;; Return the state after executing a declaration.
(define Mstate_declare
  (lambda (stmt state ctx)
    (if (= 3 (length stmt))
        ;; This is declaration AND assignment.
        (state-add (Mstate (caddr stmt) state ctx)
                   (cadr stmt)
                   (Mvalue (caddr stmt) state ctx))
        ;; This is just declaration.
        (state-add state (cadr stmt) 'undefined))))

(define Mstate_assign
  (lambda (stmt state ctx)
    (state-update state (cadr stmt) (Mvalue (caddr stmt) state ctx))))

(define Mstate_return
  (lambda (stmt state ctx)
    ((ctx-return ctx) (Mvalue (cadr stmt) state ctx))))

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
  (lambda (block state ctx)
    (if (null? block)
        state
        (Mstate_stmtlist (cdr block)
                         (Mstate (car block) state ctx)
                         ctx))))

;; Execute a block of statements.  This is different from a statement list in
;; that it adds a layer to the state, then removes it.
(define Mstate_block
  (lambda (block state ctx)
    (remove-layer
     (Mstate_stmtlist (cdr block) (add-layer state)
                      ;; Modify the break and continue
                      ;; continuations so that they remove the
                      ;; correct number of layers when they fire.
                      (ctx-new
                       (ctx-return ctx)
                       (lambda (s) ((ctx-break ctx) (remove-layer s)))
                       (lambda (s) ((ctx-continue ctx) (remove-layer s))))))))

;; Executes a while statement.
(define Mstate_while
  (lambda (stmt state ctx)
    ;; Create the new break continuation.
    (call/cc
     (lambda (break_new)
       (letrec
           ((loop (lambda (condition body state)
                    (if (Mboolean condition state (ctx-break-set ctx break_new))
                        ;; If the loop condition is true, tail recursively loop
                        ;; again.
                        (loop condition body
                              ;; Create a continue continuation
                              (call/cc (lambda (continue_new)
                                         (Mstate body state
                                                 (ctx-new (ctx-return ctx)
                                                          break_new
                                                          continue_new)))))
                        state))))
         ;; Execute the inner loop:
         (loop (cadr stmt) (caddr stmt) state))))))

;; Binds the name of this function to the closure
 ; The given funcdecl has the form:
 ; function a(x, y) { return x + y } => (function a (x y) ((return (+ x y)))
(define Mstate_funcdecl
  (lambda (funcdecl state ctx)
    (let ((fname (cadr funcdecl)))
      (state-add state fname
                 (list (caddr funcdecl) ; Parameter list
                       (cadddr funcdecl) ; Body
                       (lambda (state) ; Function to create the appropriate environment
                         (trim-state fname state)))))))

;; Get the state for a function call.
(define Mstate_funccall
  (lambda (funccall state ctx)
    (begin
      (Mvalue funccall state ctx)
      state)))


;; Return the state after executing any parse tree fragment.
(define Mstate
  (lambda (stmt state ctx)
    (cond
     ((null? stmt) state)
     ((list? stmt) (cond
                    ((list? (car stmt)) (Mstate_stmtlist stmt state ctx))
                    ((eq? 'begin (car stmt)) (Mstate_block stmt state ctx))
                    ((eq? 'var (car stmt)) (Mstate_declare stmt state ctx))
                    ((eq? '= (car stmt)) (Mstate_assign stmt state ctx))
                    ((eq? 'if (car stmt)) (Mstate_if stmt state ctx))
                    ((eq? 'return (car stmt)) (Mstate_return stmt state ctx))
                    ((eq? 'break (car stmt)) ((ctx-break ctx) state))
                    ((eq? 'continue (car stmt)) ((ctx-continue ctx) state))
                    ((eq? 'while (car stmt)) (Mstate_while stmt state ctx))
                    ((eq? 'function (car stmt)) (Mstate_funcdecl stmt state ctx))
                    ((eq? 'funcall (car stmt)) (Mstate_funccall stmt state ctx))
                    (else state)))
     (else state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overall interpreter functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interpret from the given filename, and return its value.
(define interpret
  (lambda (filename)
    (return_val (let* ((err (lambda (v) (error "Can't return/break/continue here.")))
                       (state (Mstate (parser filename) (state-new) (ctx-new err err err))))
                  (call/cc
                   (lambda (return)
                     (Mvalue '(funcall main) state (ctx-new return err err))))))))
