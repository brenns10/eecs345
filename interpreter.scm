;; EECS 345 Project Part 4
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

;; index-of: Return the index of an atom in a list (doesn't work for numbers).
;; I implemented this using a YC + CPS, cause why not?
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

;; list-set: Return the given list with the value at the given index replaced by
;; the given value.
(define list-set
  (lambda (list idx val)
    (if (= 0 idx)
        (cons val (cdr list))
        (cons (car list) (list-set (cdr list) (- idx 1) val)))))

;; Helper method to handle the fact that return statements should return
;; the atoms 'true or 'false rather than #t and #f
(define return_val
  (lambda (stmt)
    (cond
      ((eq? stmt #t) 'true)
      ((eq? stmt #f) 'false)
      (else stmt))))

;; Take a list of items and return a list of them, boxed.
(define box-list
  (lambda (l)
    (map box l)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layer/Environment functions:  '((var_name1 var_name2) (var_value1 var_value2))
;; - There are two interfaces defined here - layer and environment.
;; - Layer is blind to the existence of boxes, and so it's very useful to the
;;   state, which handles all the boxing and unboxing for you.
;; - Environment knows about boxes, and it's good for use in class/instances.
;; - There's no difference in the data structure, or how they search for stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A new layer/environment.
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

;; Lookup a binding in an environment, and return it unboxed.
(define env-lookup
  (lambda (env var)
    (let ((val (env-lookup-box env var)))
      (if (eq? val 'not_found)
          'not_found
          (unbox val)))))

;; Update a binding in an environment.
(define env-update
  (lambda (env var new)
    (let ((box (layer-lookup env var)))
      (if (eq? box 'not_found)
          (error "Variable binding not found.")
          (set-box! box new)))))

;; Return true if a variable is present in an environment/layer.
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

;; Return true if a variable is a member of a state.
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
;; - 'class
;; - Parent class, or 'null.
;; - Class name
;; - Class field environment.
;; - Method environment.
;; - Instance field names.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return a new class.  If the parent is not 'null, it will populate the fields,
;; methods, and instance fields with the parent's.
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
              (layer-new) ;; use layer because there will be no boxing
              (class-instance-names parent)))))

;; Functions for accessing items in a class.
(define class-parent cadr)
(define class-name caddr)
(define class-fields cadddr)
(define class-methods (lambda (l) (list-ref l 4)))
(define class-instance-names (lambda (l) (list-ref l 5)))

;; The following functions are used to modify a class.  They return new class
;; objects, since classes are immutable here in Scheme-land.  They should only
;; be used when building a class (doing Mclass stuff).
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
;; - 'list
;; - The class
;; - The instance field values.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A new instance.  This will probably change when we actually start doing
;; instances in Part 5.
(define inst-new
  (lambda (cls)
    (list 'inst cls '())))

;; Acessors for instances.
(define inst-class cadr)
(define inst-values caddr)

;; To modify an instance.
(define inst-values-set
  (lambda (inst values)
    (list 'inst (inst-class inst) values)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context Functions (a context contains all the damn continuations)
;; => The context is a list: '(return break continue class inst)
;; => I made it like this so we don't have to change function signatures every
;;    damn time!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; New context - contains all caller-defined values.  This one should be
;; avoided, since it requires knowing the exact number of things in the context,
;; which will change and break code.
(define ctx-new list)

;; New context - contains some nice defaults.
(define ctx-default
  (lambda ()
    (list (lambda (v) (error "You can't return here!"))
          (lambda (v) (error "You can't break here!"))
          (lambda (v) (error "You can't continue here!"))
          'null 'null
          (lambda (v) (error "Unhandled exception!")))))

;; The functions for accessing items in the context.
(define ctx-return car)
(define ctx-break cadr)
(define ctx-continue caddr)
(define ctx-class cadddr)
(define ctx-inst (lambda (l) (list-ref l 4)))
(define ctx-throw (lambda (l) (list-ref l 5)))

;; The functions for modifying items in the context.
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

(define ctx-throw-set
  (lambda (ctx throw)
    (list-set ctx 5 throw)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous Object-Oriented Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lookup a variable's box given its name, and the state/class/instance.  If the
;; variable name was dotted, you can simply pass (state-new) to this function to
;; skip looking in the state.
(define variable-lookup
  (lambda (varname state cls inst)
    (cond
     ;; Lookup in the state.
     ((state-member? state varname) (state-lookup-box state varname))
     ;; Else, lookup in the class static fields.
     ((env-member? (class-fields cls) varname) (env-lookup-box (class-fields cls) varname))
     ;; Lookup in the instance, if it exists.
     ((and (not (eq? 'null inst)) ; don't attempt to lookup if no instance
           (env-member? (list (car (class-instance-names cls)) (inst-values)) varname))
      (env-lookup-box (list (car (class-instance-names cls)) (inst-values)) varname))
     (else 'not_found))))

;; Lookup a function from the environment, or the class (or instance?).
(define function-lookup
  (lambda (varname state cls inst)
    (cond
     ((state-member? state varname) (state-lookup state varname))
     ((env-member? (class-methods cls) varname) (env-lookup (class-methods cls) varname))
     (else (error "Function name not found.")))))

;; This takes a variable (could be a number, boolean, class, instance, function,
;; or something else) and returns its instance/class pair.  Since numbers,
;; booleans, functions don't have instances or classes, they raise errors (since
;; you can't use dot on them).
(define inst-class-of-variable
  (lambda (var state ctx)
    (cond
     ((not (list? var)) (error "Dot operator may only be applied to classes and instances."))
     ((eq? (car var) 'class) (list 'null var))
     ((eq? (car var) 'inst) (list var (inst-class var)))
     (else (error "Dot operator may only be applied to classes and instances.")))))

;; This function takes a symbol (e.g. a variable, class, function call, or
;; keyword) and converts it into an appropriate list pair (class, instance).
;; This is where you would implement something like this or super.
(define dot-inst-class
  (lambda (lhs state ctx)
    (if (list? lhs)
        ;; The lhs is a list, so there is some sort of parse tree fragment to
        ;; deal with.
        (cond
         ;; Function call!
         ((eq? (car lhs) 'funcall) (inst-class-of-variable (Mvalue_funccall lhs state ctx)
                                                           state ctx))
         ;; Nested dot.  Should fail spectacularly if the variable is not a
         ;; class or an instance.
         ((eq? (car lhs) 'dot) (inst-class-of-variable (unbox (lookup-dot-var lhs state ctx))
                                                       state ctx)))
        ;; The lhs is an atom, so we're dealing with a keyword or variable
        (let ((lookup (variable-lookup lhs state (ctx-class ctx) (ctx-inst ctx))))
          (cond
           ((eq? lhs 'this) (list (ctx-inst ctx) (inst-class (ctx-inst ctx))))
           ((eq? lhs 'super) (list (ctx-inst ctx) (class-parent (ctx-class ctx))))
           ((eq? 'not_found lookup) (error "Not found."))
           (else (inst-class-of-variable (unbox lookup) state ctx)))))))

;; This helper function looks up the function corresponding to a dot expression.
;; First, it resolves the dot by calling dot-inst-class, then, it looks up the
;; function using that instance and class (but not the state).
(define lookup-dot-func
  (lambda (dotexpr state ctx)
    (let ((inst-class (dot-inst-class (cadr dotexpr) state ctx)))
      (function-lookup (caddr dotexpr) (state-new) (cadr inst-class) (car inst-class)))))

;; This universal function lookup takes any expression that resolves to a
;; function name (either a function name, or a dot expression) and returns the
;; closure corresponding to it.
(define lookup-func
  (lambda (expr state ctx)
    (if (list? expr)  ;; If the expression is a list, then it must be a dot.
        (lookup-dot-func expr state ctx)
        (function-lookup expr state (ctx-class ctx) (ctx-inst ctx)))))

;; This helper function looks up the variable corresponding to a dot expression,
;; by the same process as lookup-dot-var.
(define lookup-dot-var
  (lambda (dotexpr state ctx)
    (let ((inst-class (dot-inst-class (cadr dotexpr) state ctx)))
      (variable-lookup (caddr dotexpr) (state-new) (cadr inst-class) (car inst-class)))))

;; This universal function takes any expression that resolves to a variable and
;; returns the box corresponding to it.  It's pretty awesome.
(define lookup-var
  (lambda (expr state ctx)
    (if (list? expr) ;; If the expression is a list, then it must be dotted.
        (lookup-dot-var expr state ctx)
        (variable-lookup expr state (ctx-class ctx) (ctx-inst ctx)))))


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
                                              ; name- it's a macro
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
      (else (error "Unrecognized unary operator: " op)))))

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

;; Returns the value of an assignment operation.
(define Mvalue_assign
  (lambda (expr state ctx)
    (let* (;; We have to lookup the box first (it may not matter now, but if
           ;; there were arrays there would be side effect issues without it).
           (box (lookup-var (cadr expr) state ctx))
           ;; Then, we compute the value to store in the box.
           (value (Mvalue (caddr expr) state ctx)))
      ;; Finally, we set the box.
      (set-box! box value)
      ;; And return the value.
      value)))

;; Returns the value of a parse tree fragment which is just an atom (could be
;; either a variable or literal).
(define Mvalue_atom
  (lambda (expr state ctx)
    (cond
      ((or (boolean? expr) (number? expr)) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((eq? 'undefined (Mvalue_var expr state ctx))
       (error "Use of undefined variable:" expr))
      (else (Mvalue_var expr state ctx)))))

;; This function takes a formal parameter list and an actual parameter list, and
;; then converts this into a state layer.  It handles references properly.
(define new-layer-from-arglist
  (lambda (formal actual state ctx)
    (cond
     ((and (null? formal) (null? actual)) (layer-new))
     ((or (null? formal) (null? actual)) (error "Incorrect number of args."))
     ((eq? '& (car formal)) (add-to-layer (new-layer-from-arglist (cddr formal) (cdr actual)
                                                                  state ctx)
                                          (cadr formal)
                                          (lookup-var (car actual) state ctx)))
     (else (add-to-layer (new-layer-from-arglist (cdr formal) (cdr actual)
                                                 state ctx)
                         (car formal) (box (Mvalue (car actual) state ctx)))))))

;; Return the result of a function call.
(define Mvalue_funccall
  (lambda (funccall state ctx)
    (let* (;; First, get the closure for this function.
           (closure (lookup-func (cadr funccall) state ctx))
           ;; Then, call the function to get the new environment.
           (outerenv ((caddr closure) state))
           ;; Then, add on the new layer, constructed form arguments.
           (newstate (cons (new-layer-from-arglist (car closure) (cddr funccall)
                                                   state ctx)
                           outerenv))
           (err (lambda (v) (error "Can't break or continue here."))))
      ;; We need a return continuation so we can get the return value.
      (call/cc
       (lambda (return)
         ;; Then, just execute each statement, with all the new stuff
         (Mstate_stmtlist (cadr closure) newstate (ctx-class-set ; Set the class
                                                   (ctx-return-set ; Set the return
                                                    (ctx-break-set  ; Set break
                                                     (ctx-continue-set ; Set continue
                                                      ctx err)
                                                     err)
                                                    return)
                                                   ((cadddr closure) state))))))))

;; Returns the value of a variable.  It could be in the function's execution
;; environment, or it could be in the class's static or instance environments.
(define Mvalue_var
  (lambda (expr state ctx)
    (unbox (variable-lookup expr state (ctx-class ctx) (ctx-inst ctx)))))

;; Returns the value of a dot expression.  This can only be a variable access,
;; because if it were a function call, it would look like:
;;   (funcall (dot A main))
(define Mvalue_dot
  (lambda (expr state ctx)
    (unbox (lookup-dot-var expr state ctx))))

;; Let's create some objects!  -- '(new class-name)
(define Mvalue_new
  (lambda (expr state ctx)
    (let ((class (state-lookup state (cadr expr))))
      (if (not (and (list? class) (eq? (car class) 'class)))
          (error "Not a class: " (cadr expr))
          (inst-values-set (inst-new class) (box-list (cadr (class-instance-names class))))))))

;; Return the value of any parse tree fragment!
(define Mvalue
  (lambda (expr state ctx)
    (cond
     ((list? expr) (cond
                    ((eq? '= (car expr)) (Mvalue_assign expr state ctx))
                    ((eq? 'funcall (car expr)) (Mvalue_funccall expr state ctx))
                    ((eq? 'dot (car expr)) (Mvalue_dot expr state ctx))
                    ((eq? 'new (car expr)) (Mvalue_new expr state ctx))
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

;; Return the state after assigning a value to a variable.
(define Mstate_assign
  (lambda (stmt state ctx)
    (begin (Mvalue_assign stmt state ctx) state)))

;; Executes the return continuation!!!
(define Mstate_return
  (lambda (stmt state ctx)
    ((ctx-return ctx) (Mvalue (cadr stmt) state ctx))))

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

;; Executes a while statement.  What a mess of continuations!
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

;; Get the state for a function call.  This simply delegates that responsibility
;; to Mvalue_funccall.  This works because the side effects are maintained with
;; boxes.  We just throw away the return value that Mvalue gives us.
(define Mstate_funccall
  (lambda (funccall state ctx)
    (begin
      (Mvalue funccall state ctx)
      state)))

;; Takes a list '(finally [block]) or '() and returns a function that will
;; evaluate the finally.  The finally function takes a value and returns the
;; same value, but executes the finally block.
(define create-finally
  (lambda (l state ctx)
    (if (null? l)
        ;; If no finally, keep going.
        (lambda (thrown) thrown)
        ;; If there is a finally, we execute it.  We need to cons begin because
        ;; Mstate_block expects '(begin ....)
        (lambda (thrown) (begin (Mstate_block (cons 'begin (cadr l)) state ctx)
                                thrown)))))

;; Takes a list '(catch (varname) [block]) or '() and creates a function that
;; will evaluate the catch.  The catch function takes a value and returns a
;; state.
(define create-catch
  (lambda (l finally continuation state ctx)
    (if (null? l)
        ;; If there is no catch block, return a lambda that executes the old throw
        ;; after the finally block.
        (lambda (thrown)
          ((ctx-throw ctx) (finally thrown)))
        ;; If there is a catch block, return a lambda that executes the
        ;; continuation on the catch and finally block.
        (lambda (thrown)
          (continuation (Mstate_stmtlist (caddr l)
                                         (cons (add-to-layer (layer-new) (caadr l) (box thrown))
                                               state)
                                         ctx))))))

;; Update the context with a new throw continuation, as well as all of the old
;; return/break/continue's wrapped with the finally.
(define update-context
  (lambda (ctx catch finally)
    (ctx-return-set
     (ctx-break-set
      (ctx-continue-set
       (ctx-throw-set ctx catch)
       (lambda (v) ((ctx-continue ctx) (finally v))))
      (lambda (v) ((ctx-break ctx) (finally v))))
     (lambda (v) ((ctx-return ctx) (finally v))))))

;; Mstate for a try/catch?/finally? block.
(define try-body cadr)
(define catch-block caddr)
(define finally-block cadddr)
(define Mstate_try
  (lambda (stmt state ctx)
    (let ((finally (create-finally (finally-block stmt) state ctx)))
      (finally
       (call/cc
        (lambda (c)
          (let* ((catch (create-catch (catch-block stmt) finally c state ctx))
                 (newctx (update-context ctx catch finally)))
            ;; We need to cons 'begin because Mstate_block expects '(begin ...)
            (Mstate_block (cons 'begin (try-body stmt)) state newctx))))))))

(define Mstate_throw
  (lambda (stmt state ctx)
    ((ctx-throw ctx) (Mvalue (cadr stmt) state ctx))))


;; Return the state after executing any function code.
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
                    ((eq? 'try (car stmt)) (Mstate_try stmt state ctx))
                    ((eq? 'throw (car stmt)) (Mstate_throw stmt state ctx))
                    (else state)))
     (else state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mclass functions: These functions interpret the blocks of code within
;; classes.  Each function should return an updated version of the class we are
;; currently parsing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This function interprets a static variable declaration.
(define Mclass_staticdeclare
  (lambda (stmt state ctx)
    (let* ((class (ctx-class ctx)))
      (class-fields-set
       class
       (env-add (class-fields class)
                (cadr stmt)
                (if (= 3 (length stmt)) ;; Allow for assignment.
                    (Mvalue (caddr stmt) state ctx)
                    'undefined))))))

;; This function interprets an instance variable declaration.
(define Mclass_declare
  (lambda (stmt state ctx)
    (let* ((class (ctx-class ctx)))
      (class-instance-names-set
       class
       (add-to-layer (class-instance-names class)
                     (cadr stmt)
                     (if (= 3 (length stmt)) ;; Allow for assignment.
                         (Mvalue (caddr stmt) state ctx)
                         'undefined))))))


;; This function interprets a static function declaration.  Compare to
;; Mstate_funcdecl.  There was no real way to share the code, and their
;; functionality is different enough that I'm not too concerned.
(define Mclass_funcdecl
  (lambda (funcdecl state ctx)
    (let* ((fname (cadr funcdecl))   ; The function name.
           (cls (ctx-class ctx))     ; The class we are building.
           (cname (class-name cls))) ; The name of the class we are building.
      ;; Basically, we return a new version of the class, with the class method
      ;; list updated.
      (class-methods-set
       cls
       (env-add (class-methods cls)
                fname
                (list (caddr funcdecl)  ; Parameter list
                      (cadddr funcdecl) ; Body
                      (lambda (state)   ; Function to create environment.
                        (let ((class (state-lookup state cname)))
                          (cons (class-methods class)
                                (cons (class-fields class)
                                      (trim-state cname state)))))
                      (lambda (state)   ; Function to get class from a state.
                        (state-lookup state cname))))))))

;; This is like the big Mstate function, but it reads each statement in a class
;; declaration, and returns the class after being updated.  Mclass dispatches
;; calls to its subfunctions.
(define Mclass
  (lambda (stmt state ctx)
    (cond
     ((null? stmt) (ctx-class ctx))
     ((list? stmt) (cond
                    ((eq? 'static-function (car stmt)) (Mclass_funcdecl stmt state ctx))
                    ((eq? 'function (car stmt)) (Mclass_funcdecl stmt state ctx))
                    ((eq? 'static-var (car stmt)) (Mclass_staticdeclare stmt state ctx))
                    ((eq? 'var (car stmt)) (Mclass_declare stmt state ctx))
                    (else (error "Invalid statement in class declaration."))))
     (else (ctx-class ctx)))))

;; This function processes a list of statements in a class.  It takes the output
;; of each Mclass call and puts it into the input of the next, and then finally
;; returns the completely constructed class.
(define Mclass_stmtlist
  (lambda (block state ctx)
    (if (null? block)
        (ctx-class ctx)
        (Mclass_stmtlist (cdr block)
                         state
                         (ctx-class-set ctx (Mclass (car block) state ctx))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outer Interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This function reads a class declaration, and binds it to the name of the
;; class in the state.  It returns the state after interpreting the declaration.
(define Mstate_class
  (lambda (stmt state ctx)
    (let* ((name (cadr stmt))
           (extends (caddr stmt))
           (parent (if (null? extends) 'null (state-lookup state (cadr extends))))
           (body (cadddr stmt))
           (class (Mclass_stmtlist body state (ctx-class-set ctx (class-new parent name)))))
      (state-add state name class))))

;; This function interprets at the global scope.  It calls Mstate_class on each
;; class declaration (and raises an error on anything else).  It returns a state
;; containing the class names and class data structures.
(define outer-interpreter
  (lambda (block state ctx)
    (cond
     ((null? block) state)
     ((eq? (car (car block)) 'class)
      (outer-interpreter (cdr block) (Mstate_class (car block) state ctx) ctx))
     (else (error "You may only declare classes in the global scope.")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main interpret function!!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interprets a file, given a class to call main on.
(define interpret
  (lambda (filename class)
    (return_val
     (let ((state (outer-interpreter (parser filename) (state-new) (ctx-default))))
       (call/cc
        (lambda (return)
          (Mvalue (list 'funcall (list 'dot class 'main)) state
                  (ctx-return-set (ctx-default) return))))))))
