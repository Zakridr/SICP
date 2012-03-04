#lang racket

; ex 4.1

(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      empty
      (let ((first-val (eval (first-operand exps) env)))
        (cons first-val
              (list-of-values (rest-operands exps) env)))))

(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      empty
      (let ((rest-vals (eval (rest-operands exps) env)))
        (cons (eval (first-operands exps) env)
              (rest-vals)))))