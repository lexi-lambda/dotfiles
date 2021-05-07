#lang s-exp framework/keybinding-lang

(require string-constants)

(define (rebind key command)
  (keybinding
   key
   (procedure-rename (λ (editor evt)
                       (send (send editor get-keymap) call-function
                             command editor evt #t))
                     (string->symbol (string-append command " [custom]")))))

(rebind "c:m:b" (string-constant cs-jump-to-binding))
(rebind "c:m:d" (string-constant cs-jump-to-definition))
(rebind "c:m:[" (string-constant cs-jump-to-previous-bound-occurrence))
(rebind "c:m:]" (string-constant cs-jump-to-next-bound-occurrence))
