#lang racket

(require racket/generator)

(define (cons-ints a b)
  (+ (* a 10) b))

(define (safe-div a b)
  (if (= b 0) +nan.f (/ a b)))

(define op->string (hash '+ "+"
                         '- "-"
                         '* "*"
                         '/ "/"))

(define op->proc (hash '+ +
                       '- -
                       '* *
                       '/ safe-div
                       'cons cons-ints))

(define (compute source op-stack)
  (let loop ([op-stack op-stack] [compute-stack null] [source-stack source])
    (match (vector op-stack compute-stack source-stack)
      [(vector (list) (list* peek _) _) peek]
      [(vector (list* 'push op-stack) _ (list* item source-stack))
       (loop op-stack (cons item compute-stack) source-stack)]
      [(vector (list* op op-stack) (list* x y compute-stack) _)
       (loop op-stack (cons ((hash-ref op->proc op) x y) compute-stack) source-stack)])))

(define (show source op-stack)
  (let loop ([op-stack op-stack] [compute-stack null] [source-stack source])
    (match (vector op-stack compute-stack source-stack)
      [(vector (list) (list* content _) _) content]
      [(vector (list* 'push op-stack) _ (list* item source-stack))
       (loop op-stack (cons item compute-stack) source-stack)]
      [(vector (list* op op-stack) (list* x y compute-stack) _)
       (loop op-stack (cons (if (eq? op 'cons)
                                (format "~a~a" x y)
                                (format "(~a ~a ~a)" (hash-ref op->string op) x y)) compute-stack) source-stack)])))

(define-syntax-rule (yield! it)
  (for ([i it])
    (yield i)))

(define (prove-all source target)
  (define len (- (* (length source) 2) 1))
  (let loop ([a 0] [b 0] [op-stack null])
    (cond
      [(= (+ a b) len)
       (let* ([op-stack (reverse op-stack)]
              [result (compute source op-stack)])
         (if (= result target)
             (stream op-stack)
             empty-stream))]
      [else
       (let* ([return empty-stream]
              [return (if (<= a (/ len 2))
                          (stream-append (loop (+ a 1) b (cons 'push op-stack)) return)
                          return)]
              [return (if (> a (+ b 1))
                          (for/fold ([return return])
                                    ([op '(+ - * /)])
                            (stream-append (loop a (+ b 1) (cons op op-stack)) return))
                          return)]
              [return (for/fold ([return return])
                                ([i (in-naturals)]
                                 #:break (or (> (+ a i 1) (quotient len 2)) (< a (- b 2))))
                        (stream-append (loop (+ a i 2) (+ b i 1)
                                             (append (build-list (+ i 1) (lambda (i) 'cons))
                                                     (build-list (+ i 2) (lambda (i) 'push))
                                                     op-stack)) return))])
         return)])))

(define (prove source target)
  (define result (prove-all source target))
  (or (stream-empty? result) (stream-first result)))

(define (get-ints s)
  (reverse (for/list ([c s]
                      #:when (char-numeric? c))
             (bitwise-xor (char->integer c) 48))))

(define digest (make-parameter #f))
(define-values (target source)
  (command-line
   #:once-each
   [("-d" "--digest") "" (digest #t)]
   #:args (target [source "114514"])
   (values (string->number target) (get-ints source))))

(define past (current-milliseconds))
(cond
  [(digest)
   (define result (prove source target))
   (if result
       (printf "(= ~a ~a)\n" target (show source result))
       (displayln "No result found."))]
  [else
   (define result (prove-all source target))
   (define count (for/sum ([r result])
                   (printf "(= ~a ~a)\n" target (show source r))
                   1))
   (printf "~a results found\n" count)])
(printf "Fixed within ~a milliseconds\n" (- (current-milliseconds) past))
