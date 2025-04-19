#lang br/quicklang

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFER
;; The BF buffer is simulated using a vector of size BF-BUFFER-SIZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BF-BUFFER-SIZE 30000)

;; Vector Number -> Number
;; Returns the value of arr[ptr]
(define (current-byte arr ptr) (vector-ref arr ptr))

;; Vector Number -> Vector
;; EFFECT: Mutates the value at arr[ptr] to val
(define (set-current-byte arr ptr val)
  (vector-set! arr ptr val)
  arr)

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
      (let* ([arr (first state)]
             [ptr (second state)])
        (for/fold ([current-state state])
                  ([i (in-naturals)]
                   #:break (zero? (apply current-byte
                                         current-state)))
          (fold-funcs current-state (list OP-OR-LOOP-ARG ...))))))
(provide bf-loop)

;; State (listof Operation) -> State
;; Applies the list of BF-FUNC to the state and returns the final state
(define (fold-funcs initial-state bf-funcs)
  (for/fold ([current-state initial-state])
            ([bf-func (in-list bf-funcs)])
    (bf-func current-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BF Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Operation is State -> State

;; Jump table for bf-op -> Operation
(define-macro-cases bf-op
  [(bf-op ">") #'gt]
  [(bf-op "<") #'lt]
  [(bf-op "+") #'plus]
  [(bf-op "-") #'minus]
  [(bf-op ".") #'period]
  [(bf-op ",") #'comma])
(provide bf-op)

;; Operation definitions

(define (gt state)
  (let* ([arr (first state)]
         [ptr (second state)])
    (list arr (add1 ptr))))

(define (lt state)
  (let* ([arr (first state)]
         [ptr (second state)])
    (list arr (sub1 ptr))))

(define (plus state)
  (let* ([arr (first state)]
         [ptr (second state)])
    (list
     (set-current-byte arr ptr (add1 (current-byte arr ptr)))
     ptr)))

(define (minus state)
  (let* ([arr (first state)]
         [ptr (second state)])
    (list
     (set-current-byte arr ptr (sub1 (current-byte arr ptr)))
     ptr)))

(define (period state)
  (let* ([arr (first state)]
         [ptr (second state)])
    (write-byte (current-byte arr ptr))
    (list arr ptr)))

(define (comma state)
  (let* ([arr (first state)]
         [ptr (second state)])
    (list (set-current-byte arr ptr (read-byte)) ptr)))
