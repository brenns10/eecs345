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

(define list-set
  (lambda (list idx val)
    (if (= 0 idx)
        (cons val (cdr list))
        (cons (car list) (list-set (cdr list) (- idx 1) val)))))


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
  (lambda (env var value) (add-to-layer env var (box value))))

;; Lookup the binding for var in the state layer.
(define layer-lookup
  (lambda (layer var)
    (let ((idx (index-of (car layer) var)))
      (if (= -1 idx)
          'not_found
          (list-ref (cadr layer) (- (length (cadr layer)) idx 1))))))
(define env-lookup-box layer-lookup)
(define env-lookup
  (lambda (env var)
    (let ((val (env-lookup-box env var)))
      (if (eq? val 'not_found)
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
;; Class functions: A class is a list containing the following (in order):
;; - Parent class, or 'null.
;; - Class name
;; - Class field environment.
;; - Method environment.
;; - Instance field names.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define class-new
  (lambda (parent name)
    (list 'class parent name
          (if (eq? parent 'null)
              (env-new)
              (class-fields parent))
          (if (eq? parent 'null)
              (env-new)
              (class-methods parent))
          (if (eq? parent 'null)
              '()
              (class-instance-names parent)))))

(define class-parent cadr)
(define class-name caddr)
(define class-fields cadddr)
(define class-methods (lambda (l) (list-ref l 4)))
(define class-instance-names (lambda (l) (list-ref l 5)))

(define class-fields-set
  (lambda (cls fields)
    (list-set cls 3 fields)))

(define class-methods-set
  (lambda (cls methods)
    (list-set cls 4 methods)))

(define class-instance-names-set
  (lambda (cls instance-names)
    (list-set cls 5 instance-names)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instance functions: An instance is a list containing:
;; - The class
;; - The instance field values.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define inst-new
  (lambda (cls)
    (list 'inst cls '())))

(define inst-cls cadr)
(define inst-values caddr)

(define inst-values-set
  (lambda (inst values)
    (list 'inst (inst-cls) values)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context Functions (a context contains all the damn continuations)
;; => The context is a list: '(return break continue class inst)
;; => I made it like this so we don't have to change function signatures every
;;    damn time!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; New continuation - contains all caller-defined values
(define ctx-new list)

;; The functions for accessing items in the continuation.
(define ctx-return car)
(define ctx-break cadr)
(define ctx-continue caddr)
(define ctx-class cadddr)
(define ctx-inst (lambda (l) (list-ref l 4)))

;; The functions for modifying items in the continuation.
(define ctx-return-set
  (lambda (ctx return)
    (cons return (cdr ctx))))

(define ctx-break-set
  (lambda (ctx break)
    (list-set ctx 1 break)))

(define ctx-continue-set
  (lambda (ctx continue)
    (list-set ctx 2 continue)))

(define ctx-class-set
  (lambda (ctx class)
    (list-set ctx 3 class)))

(define ctx-inst-set
  (lambda (ctx inst)
    (list-set ctx 4 inst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous Object-Oriented Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A universal lookup function!  Looks up varname in the state, then the class
;; fields, then the instance fields.  Pass (state-new) if you want to bypass
;; looking in the state (eg. if you received the dot operator).  Returns the
;; BOXED value, so that if you are doing assignment, you can assign to it.
(define lookup-var
  (lambda (varname state cls inst)
    (cond
     ;; Lookup in the state.
     ((state-member? state varname) (state-lookup-box state varname))
     ;; Else, lookup in the class static fields.
     ((env-member? (class-fields cls) varname) (env-lookup-box (class-fields cls) varname))
     ;; Lookup in the instance, if it exists.
     ((and (not (eq? 'null inst)) ; don't attempt to lookup if no instance
           (env-member? (list (class-instance-names cls) (inst-values)) varname))
      (env-lookup-box (list (class-instance-names cls) (inst-values)) varname))
     (else 'not_found))))

(define lookup-func
  (lambda (varname state cls inst)
    ;; (display "\nLooking up function\n")
    ;; (display varname) (display "\n")
    ;; (display "IN: state:") (display state) (display "\n")
    ;; (display "IN: cls:") (display cls) (display "\n")
    ;; (display "IN: inst:") (display inst) (display "\n")
    (cond
     ((state-member? state varname) (state-lookup state varname))
     ((env-member? (class-methods cls) varname) (env-lookup (class-methods cls) varname))
     (else (error "Function name not found.")))))

(define dot-inst-class
  (lambda (lhs state ctx)
    (let ((lookup (lookup-var lhs state (ctx-class ctx) (ctx-inst ctx))))
      (cond
       ((eq? lhs 'this) (list (ctx-inst ctx) (inst-class (ctx-inst ctx))))
       ((eq? lhs 'super) (list (ctx-inst ctx) (class-parent (ctx-class ctx))))
       ((eq? 'not_found lookup) (error "Not found."))
       ((eq? 'class (car (unbox lookup))) (list 'null (unbox lookup)))
       ((eq? 'inst (car (unbox lookup))) (list (unbox lookup) (inst-class (unbox lookup))))
       ((eq? (car lhs) 'funcall)
        (let ((result (Mvalue_funccall lhs state ctx)))
          (list result (inst-class result))))))))

(define lookup-dot-func
  (lambda (dotexpr state ctx)
    (let ((inst-class (dot-inst-class (cadr dotexpr) state ctx)))
      (lookup-func (caddr dotexpr) state (cadr inst-class) (car inst-class)))))

(define lookup-dot-var
  (lambda (dotexpr state ctx)
    (let ((inst-class (dot-inst-class (cadr dotexpr state ctx))))
      (lookup-var (caddr dotexpr) state (cadr inst-class) (car inst-class)))))



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
      ((eq? 'undefined (Mvalue_var expr state ctx))
       (error "Use of undefined variable."))
      (else (Mvalue_var expr state ctx)))))

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
    (let* ((closure (if (list? (cadr funccall))
                        (lookup-dot-func (cadr funccall) state ctx)
                        (lookup-func (cadr funccall) state (ctx-class ctx) (ctx-inst ctx))))
           (outerenv ((caddr closure) state))
           (newstate (cons (new-layer-from-arglist (car closure) (cddr funccall)
                                                   state ctx)
                           outerenv))
           (err (lambda (v) (error "Can't break or continue here."))))
      (call/cc
       (lambda (return)
         (Mstate_stmtlist (cadr closure) newstate (ctx-class-set
                                                   (ctx-return-set
                                                    (ctx-break-set
                                                     (ctx-continue-set
                                                      ctx err)
                                                     err)
                                                    return)
                                                   ((cadddr closure) state))))))))

(define Mvalue_var
  (lambda (expr state ctx)
    (unbox (lookup-var expr state (ctx-class ctx) (ctx-inst ctx)))))

(define Mvalue_dot
  (lambda (expr state ctx)
    (let ((inst-class (dot-inst-class (cadr expr) state ctx)))
      (unbox (lookup-var (caddr expr) (state-new) (cadr inst-class) (car inst-class))))))

;; Return the value of any parse tree fragment!
(define Mvalue
  (lambda (expr state ctx)
    (cond
     ((list? expr) (cond
                    ((eq? '= (car expr)) (Mvalue_assign expr state ctx))
                    ((eq? 'funcall (car expr)) (Mvalue_funccall expr state ctx))
                    ((eq? 'dot (car expr)) (Mvalue_dot expr state ctx))
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

(define Mclass_staticdeclare
  (lambda (stmt state ctx)
    (let* ((class (ctx-class ctx)))
      (class-fields-set
       class
       (env-add (class-fields class)
                (cadr stmt)
                (if (= 3 (length stmt))
                    (Mvalue (caddr stmt) state ctx)
                    'undefined))))))

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
                      (ctx-continue-set
                       (ctx-break-set
                        ctx
                        (lambda (s) ((ctx-break ctx) (remove-layer s))))
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
                                                 (ctx-continue-set
                                                  (ctx-break-set ctx break_new)
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
                         (trim-state fname state))
                       (lambda (state) ; Function to get this function's class
                         (ctx-class ctx)))))))

(define Mclass_staticfuncdecl
  (lambda (funcdecl state ctx)
    (let* ((fname (cadr funcdecl))
           (cls (ctx-class ctx))
           (cname (class-name cls)))
      (class-methods-set
       cls
       (env-add (class-methods cls)
                fname
                (list (caddr funcdecl) ; Parameter list
                      (cadddr funcdecl) ; Body
                      (lambda (state) ; Function to create environment.
                        (let ((class (state-lookup state cname)))
                          (cons (class-methods class)
                                (cons (class-fields class)
                                      (trim-state cname state)))))
                      (lambda (state)
                        (state-lookup state cname))))))))

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

;; This is like Mstate, but for within classes.
(define Mclass
  (lambda (stmt state ctx)
    (cond
     ((null? stmt) (ctx-class ctx))
     ((list? stmt) (cond
                    ((eq? 'static-function (car stmt)) (Mclass_staticfuncdecl stmt state ctx))
                    ((eq? 'static-var (car stmt)) (Mclass_staticdeclare stmt state ctx))
                    (else (error "You can only declare static functions and variables in a class."))))
     (else (ctx-class ctx)))))

(define Mclass_stmtlist
  (lambda (block state ctx)
;;    (display (ctx-class ctx)) (display "\n") (flush-output)
    (if (null? block)
        (ctx-class ctx)
        (Mclass_stmtlist (cdr block)
                         state
                         (ctx-class-set ctx (Mclass (car block) state ctx))))))

(define Mstate_class
  (lambda (stmt state ctx)
    (let* ((name (cadr stmt))
           (extends (caddr stmt))
           (parent (if (null? extends) 'null (state-lookup state (cadr extends))))
           (body (cadddr stmt))
           (class (Mclass_stmtlist body state (ctx-class-set ctx (class-new parent name)))))
      (state-add state name class))))


(define outer-interpreter
  (lambda (block state ctx)
    (cond
     ((null? block) state)
     ((eq? (car (car block)) 'class)
      (outer-interpreter (cdr block) (Mstate_class (car block) state ctx) ctx))
     (else (error "You may only declare classes in the global scope.")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overall interpreter functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interpret from the given filename, and return its value.
(define interpret
  (lambda (filename class)
    (return_val (let* ((err (lambda (v) (error "Can't return/break/continue here.")))
                       (state (outer-interpreter (parser filename) (state-new) (ctx-new err err err 'null 'null))))
                  (call/cc
                   (lambda (return)
;;                     (display state) (display "\n") (flush-output)
                     (Mvalue (list 'funcall (list 'dot class 'main)) state (ctx-new return err err 'null 'null))))))))
