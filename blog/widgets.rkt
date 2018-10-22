#lang racket
(require threading
         xml
         txexpr)

(provide (all-defined-out))

(define (dropdown #:button-id button-id
                  #:button-extra-classes button-extra-classes
                  #:button-label button-label
                  .
                  elements)
  (xexpr->html
   `(div ([class "dropdown"])
         (a ([class ,(~a "btn dropdown-toggle " button-extra-classes)]
             [href "#"]
             [role "button"]
             [id ,button-id]
             [data-toggle "dropdown"]
             [aria-haspopup "true"]
             [aria-expanded "false"])
            ,button-label)
         ;; add attrs to each element, then put them in a dropdown-menu div
         ,(~> (map
               (lambda (tx)
                 (attr-set* tx 'class "dropdown-item" 'aria-labelledby button-id))
               elements)
              (txexpr 'div '([class "dropdown-menu"]) _)))))


(define (project url title description)
  `(div ([class "project"])
        (a ([href ,url])
           ,title)
        (p ,description)))
