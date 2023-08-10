#lang racket

(provide (all-defined-out))

(define (safe-div a b)
  (if (= b 0) +nan.f (/ a b)))

(define op->string #hash((+ . "+")
                         (- . "-")
                         (* . "*")
                         (/ . "*")))

(define op->proc (hash '+ +
                       '- -
                       '* *
                       '/ safe-div))

(define op-precedence #hash((+ . 1) (- . 1) (* . 2) (/ . 2)))

(define (compute expr)
  (let loop ([expr expr] [compute-stack null])
    (match (vector expr compute-stack)
      [(vector '() (list return)) return]
      [(vector _ (list* (? number? a) (? number? b) (? symbol? op) compute-stack))
       (loop expr (cons ((hash-ref op->proc op) a b) compute-stack))]
      [(vector (list* item expr) _)
       (loop expr (cons item compute-stack))])))

(define (->infix expr)
  (let loop ([expr expr] [compute-stack null])
    (match (vector expr compute-stack)
      [(vector '() (list (cons return _))) return]
      [(vector _ (list* (cons contentl prl) (cons contentr prr) op compute-stack))
       (define prm (hash-ref op-precedence op))
       (define formatter (match (cons (< prl prm) (or (< prr prm) (and (= prr prm) (or (eq? op '-) (eq? op '/)))))
                           [(cons #f #f) "~a~a~a"]
                           [(cons #t #f) "(~a)~a~a"]
                           [(cons #f #t) "~a~a(~a)"]
                           [(cons #t #t) "(~a)~a(~a)"]))
       (loop expr (cons (cons (format formatter contentl (hash-ref op->string op) contentr) prm) compute-stack))]
      [(vector (list* item expr) _)
       (loop expr (cons (if (symbol? item) item (cons item 4)) compute-stack))])))

(define (prove-all source target)
  (let loop ([source source] [expr null] [op-count 0])
    (if (and (null? source) (= op-count 1))
        (if (= (compute expr) target)
            (stream expr)
            empty-stream)
        (let* ([return (if (null? source)
                           empty-stream
                           (stream-lazy (loop (cdr source) (cons (car source) expr) (+ op-count 1))))]
               [return (if (> op-count 1)
                           (for/fold ([return return])
                                     ([op (in-list '(+ - * /))])
                             (stream-append (stream-lazy (loop source (cons op expr) (- op-count 1))) return))
                           return)])
          (match (vector source expr)
            [(vector (list* a source) (list* (? number? b) expr))
             (stream-append (stream-lazy (loop source (cons (+ (* a 10) b) expr) op-count)) return)]
            [_ return])))))

(define (prove source target)
  (define result (prove-all source target))
  (if (stream-empty? result)
      #f
      (stream-first result)))

(define (get-ints s)
  (reverse (for/list ([c s]
                      #:when (char-numeric? c))
             (bitwise-xor (char->integer c) 48))))

(when (not (= (vector-length (current-command-line-arguments)) 0))
  (define digest (make-parameter #f))
  (define-values (target source)
    (command-line
     #:once-each
     [("-d" "--digest") "" (digest #t)]
     #:args (target [source "114514"])
     (values (string->number target) (get-ints source))))

  (define past (current-milliseconds))
  (define now (make-parameter 0))
  (cond
    [(digest)
     (define result (prove source target))
     (if result
         (printf "~a = ~a\n" target (->infix result))
         (displayln "No result found."))
     (now (current-milliseconds))]
    [else
     (define result (prove-all source target))
     (define exprs (remove-duplicates (for/list ([r (in-stream result)]) (->infix r))))
     (now (current-milliseconds))
     (displayln (string-join (map (curry format "~a = ~a" target) exprs) "\n"))
     (printf "~a results found\n" (length exprs))])
  (printf "Fixed within ~a milliseconds\n" (- (now) past)))
