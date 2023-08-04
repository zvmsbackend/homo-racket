#lang racket

(require racket/generator)

(define (cons-ints a b)
  (+ (* a 10) b))

(define (safe-div a b)
  (if (= b 0) +nan.f (/ a b)))

(define op->string (hash '+ "+"
                         '- "-"
                         '* "*"
                         '/ "/"
                         'cons ""))

(define op->proc (hash '+ +
                       '- -
                       '* *
                       '/ safe-div
                       'cons cons-ints))
(define op-precedence (hash '+ 1 '- 1
                            '* 2 '/ 2
                            'cons 3))

(define (compute source op-stack)
  (let loop ([op-stack op-stack] [compute-stack null] [source-stack source])
    (match (vector op-stack compute-stack source-stack)
      [(vector (list) (list* peek _) _) peek]
      [(vector (list* 'push op-stack) _ (list* item source-stack))
       (loop op-stack (cons item compute-stack) source-stack)]
      [(vector (list* op op-stack) (list* x y compute-stack) _)
       (loop op-stack (cons ((hash-ref op->proc op) x y) compute-stack) source-stack)])))

(define (->infix source op-stack)
  (let loop ([op-stack op-stack] [compute-stack null] [source-stack source])
    (match (vector op-stack compute-stack source-stack)
      [(vector (list) (list* (cons content _) _) _) content]
      [(vector (list* 'push op-stack) _ (list* item source-stack))
       (loop op-stack (cons (cons (number->string item) 4) compute-stack) source-stack)]
      [(vector (list* op op-stack) (list* (cons content1 pr1) (cons content2 pr2) compute-stack) _)
       (define prm (hash-ref op-precedence op))
       (define symbol (hash-ref op->string op))
       (define formatter (match (cons (< pr1 prm) (or (< pr2 prm) (and (= pr2 prm) (or (eq? op '-) (eq? op '/)))))
                           [(cons #f #f) "~a~a~a"]
                           [(cons #t #f) "(~a)~a~a"]
                           [(cons #f #t) "~a~a(~a)"]
                           [(cons #t #t) "(~a)~a(~a)"]))
       (loop op-stack (cons (cons (format formatter content1 symbol content2) prm) compute-stack) source-stack)])))

(define-syntax-rule (yield! it)
  (for ([i it])
    (yield i)))

(define (prove-all source target)
  (define len (- (* (length source) 2) 1))
  (sequence->stream (let loop ([a 0] [b 0] [op-stack null])
                      (in-generator
                       (cond
                         [(= (+ a b) len)
                          (let* ([op-stack (reverse op-stack)]
                                 [result (compute source op-stack)])
                            (when (= result target)
                              (yield op-stack)))]
                         [else
                          (when (<= a (/ len 2))
                            (yield! (loop (+ a 1) b (cons 'push op-stack))))
                          (when (> a (+ b 1))
                            (for ([op '(+ - * /)])
                              (yield! (loop a (+ b 1) (cons op op-stack)))))
                          (for ([i (in-naturals)]
                                #:break (or (> (+ a i 1) (quotient len 2)) (< a (- b 2))))
                            (yield! (loop (+ a i 2) (+ b i 1)
                                          (append (build-list (+ i 1) (const 'cons))
                                                  (build-list (+ i 2) (const 'push))
                                                  op-stack))))])))))

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
       (printf "~a = ~a\n" target (->infix source result))
       (displayln "No result found."))]
  [else
   (define result (prove-all source target))
   (define count (for/sum ([r result])
                   (printf "~a = ~a\n" target (->infix source r))
                   1))
   (printf "~a results found\n" count)])
(printf "Fixed within ~a milliseconds\n" (- (current-milliseconds) past))
