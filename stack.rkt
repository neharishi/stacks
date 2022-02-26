(define (make-stack)
  (let ((top-ptr '()))
   ; checks whether a stack is empty
   (define (empty?) (null? top-ptr))
   ;places an item on the stack
    (define (push! arg)
      (let ((arg (cons arg '())))
      (cond ((empty?) (set! top-ptr arg))
            (else (set-cdr! arg top-ptr)
                  (set! top-ptr arg)))))
                  
;removes and returns the item from the top of the stack
  (define (pop!)
    (let ((old (car top-ptr)))
    (cond ((empty?) ("empty stack"))
          (else (set! top-ptr (cdr top-ptr))))
    old))
    
;shows the item on the top of the stack, but does not change the stack
  (define (peek)
    (car top-ptr))
    
;shows the stack
   (define (show-stack)
     top-ptr)
     
;message passing structure
  (define (dispatch msg)
    (cond ((eq? msg 'peek) peek)
          ((eq? msg 'push!) push!)
          ((eq? msg 'show-stack) show-stack)
          ((eq? msg 'pop!) pop!)))
  dispatch))

;sample stack
(define pancakes (make-stack))
((pancakes 'push!) 'whole-wheat)
((pancakes 'push!) 'blueberry)
((pancakes 'push!) 'chocolate-chip)
((pancakes 'show-stack))
((pancakes 'peek))
((pancakes 'pop!))
((pancakes 'show-stack))

