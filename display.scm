;; Fucking display shit.
(require racket/pretty)

(define display-inst
  (lambda (inst)
    (if (eq? inst 'null)
        (display inst)
        (begin
          (display "(inst ") (display (class-name (inst-class inst)))
          (display " ") (display "(")
          (map (lambda (v) (mydisplay (unbox v)) (display " ")) (inst-values inst))
          (display ")")))))

(define mydisplay
  (lambda (object)
    (if (and (pair? object) (eqv? 'inst (car object)))
        (display-inst object)
        (display object))))

;; Define a print function for our interpreter!
(define print-function
  (create-builtin-function
   'print '(object)
   (lambda (object)
     (if (and (pair? object) (eqv? 'inst (car object)))
         (begin (display-inst object) (display "\n"))
         (pretty-print object)))))

