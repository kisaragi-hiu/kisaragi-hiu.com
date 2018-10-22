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
  `(li ([class "project"])
       (a ([class "project-link"]
           [href ,url])
          (p ([class "project-title"])
             ,title
             nbsp)
          (p ([class "project-description"])
             ,description))))

(define (collapse #:button-classes button-classes
                  #:button-label button-label
                  #:div-id div-id
                  #:div-extra-classes div-extra-classes
                  .
                  elements)
  (xexpr->html
   `(div (a ([class ,button-classes]
             [data-toggle "collapse"]
             [href ,(~a "#" div-id)]
             [role "button"]
             [aria-expanded "false"]
             [aria-controls ,div-id])
            ,button-label))
   `(div ([class ,(~a "collapse " div-extra-classes) [id ,div-id]])
         ,@elements)))

(define (collapse-button #:button-class [button-class ""]
                         #:div-id [div-id ""]
                         .
                         elements)
  (xexpr->html
   `(div (a ([class ,button-class]
             [data-toggle "collapse"]
             [href ,(~a "#" div-id)]
             [role "button"]
             [aria-expanded "false"]
             [aria-controls ,div-id])
            ,@elements))))
