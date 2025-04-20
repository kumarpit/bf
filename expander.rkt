#lang br/quicklang

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFER
;; The BF buffer is simulated using a vector of size BF-BUFFER-SIZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BF-BUFFER-SIZE 30000)

;; Vector Number -> Number
;; Returns the value of vec[ptr]
(define (current-byte vec ptr) (vector-ref vec ptr))

;; Vector Number -> Vector
;; EFFECT: Mutates the value at vec[ptr] to val
(define (set-current-byte vec ptr val)
  (vector-set! vec ptr val)
  vec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPANDER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; State is (list Vector Number)
;; Represents the current state of the buffer and position in the buffer
;; for the BF program

;; Notice that the BF module-begin doesn't perform any processing on
;; the PARSE-TREE 
(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [bf-module-begin #%module-begin]))

(define-macro (bf-program OP-OR-LOOP-ARG ...)
  #'(begin
      (define initial-state (list (make-vector BF-BUFFER-SIZE 0) 0))
      (void (fold-funcs initial-state (list OP-OR-LOOP-ARG ...)))))
(provide bf-program)

(define-macro (bf-loop "[" OP-OR-LOOP-ARG ... "]")
  #'(λ (state)
      (let* ([vec (first state)]
             [ptr (second state)])
        (for/fold ([current-state state])
                  ([i (in-naturals)]
                   #:break (zero? (apply current-byte
                                         current-state)))
          (fold-funcs current-state (list OP-OR-LOOP-ARG ...))))))
(provide bf-loop)

;; State (listof Operation) -> State
;; Applies the list of Operation to the state and returns the final state
(define (fold-funcs initial-state bf-funcs)
  (for/fold ([current-state initial-state])
            ([bf-func (in-list bf-funcs)])
    (bf-func current-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BF OPERATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Operation is State -> State

(begin-for-syntax
  (define (make-define-op-syntax-param)
    (λ (stx)
      (raise-syntax-error (syntax-e stx) "Can only be used inside define-op"))))

(require racket/stxparam)
(define-syntax-parameter vec (make-define-op-syntax-param))
(define-syntax-parameter ptr (make-define-op-syntax-param))

(define-syntax-rule (define-op name body)
  (define (name state)
    (let* ([_vec (first state)]
           [_ptr (second state)])
      (syntax-parameterize ([vec (make-rename-transformer #'_vec)])
        (syntax-parameterize ([ptr (make-rename-transformer #'_ptr)])
          body)))))

;; Jump table for `bf-op` tokens to Operation
(define-macro-cases bf-op
  [(bf-op ">") #'gt]
  [(bf-op "<") #'lt]
  [(bf-op "+") #'plus]
  [(bf-op "-") #'minus]
  [(bf-op ".") #'period]
  [(bf-op ",") #'comma])
(provide bf-op)

;; Operation definitions

(define-op gt (list vec (add1 ptr)))
(define-op lt (list vec (sub1 ptr)))
(define-op plus (list
                 (set-current-byte vec ptr (add1 (current-byte vec ptr)))
                 ptr))
(define-op minus (list
                  (set-current-byte vec ptr (sub1 (current-byte vec ptr)))
                  ptr))
(define-op period (begin
                    (write-byte (current-byte vec ptr))
                    (list vec ptr)))
(define-op comma (list (set-current-byte vec ptr (read-byte)) ptr))
