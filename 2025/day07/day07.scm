;; Advent of Code 2025 - Day 07
;; usage : guile day07.scm ../inputs/day07.txt

(define-module (day07)
  #:use-module
  (ice-9 match)
  #:use-module
  (ice-9 rdelim)
  #:use-module
  (ice-9 format)
  #:use-module
  (ice-9 q)
  #:use-module
  (srfi srfi-1)
  #:use-module
  (srfi srfi-11)
  #:use-module
  (srfi srfi-9)
  #:export
  (main indexes-of))

(use-modules (uniq))

(define-record-type <pos>
  (make-pos x y)
  pos?
  (x pos-x set-pos-x!)
  (y pos-y set-pos-y!))

(define (pos-down pos)
  (match-let ((($ <pos> x y) pos))
    (make-pos (+ x 1) y)))

(define (pos-down-left pos)
  (match-let ((($ <pos> x y) pos))
    (make-pos (+ x 1) (- y 1))))

(define (pos-down-right pos)
  (match-let ((($ <pos> x y) pos))
    (make-pos (+ x 1) (+ y 1))))

;; return all the index of char in string
(define (indexes-of char str)
  (let ((len (string-length str)))
    (let loop ((i 0) (acc '()))
      (cond
        ((= i len) (reverse acc))
        ((char=? (string-ref str i) char)
          (loop (+ i 1) (cons i acc)))
        (else
          (loop (+ i 1) acc))))))

;; transform a line into a list of position
(define (line->pos char line idx)
  (map
    (lambda (y) (make-pos idx y))
    (indexes-of char line)))

;; read the given file and pass each line through the transformer
(define (readfile filepath transformer)
  (call-with-input-file filepath
    (lambda (port)
      (let loop ((line (read-line port))
                 (acc '()))
        (if (eof-object? line)
          (reverse acc)
          (loop (read-line port)
            (cons (transformer line) acc)))))))

;; read the given file and pass the first line through header-transformer and the rest through body-transformer
;; it returns
;; - a list of transformed line where the head is the transformed first line
;; - the number of lines -> nb rows
;; - the length of the lines -> nb cols
(define (readfile-with-header filepath header-transformer body-transformer)
  (call-with-input-file filepath
    (lambda (port)
      (let ((first-line (read-line port)))
        (if (eof-object? first-line)
          '(() 0 0)
          (let* ((cols (string-length first-line))
                 (header-data (header-transformer first-line 0)))
            (let loop ((line (read-line port))
                       (idx 1)
                       (acc (list header-data)))
              (if (eof-object? line)
                (list (reverse acc) idx cols)

                (loop (read-line port)
                  (+ idx 1)
                  (cons (body-transformer line idx) acc))))))))))

;; in? is a function that check if a position is inside the grid or not
(define (step1 in? work splitters acc)
  (if (uniq-empty? work)
    acc
    (let ((item (uniq-pop! work)))
      (if (hash-ref splitters item)
        (begin
          (let
            ((down-left (pos-down-left item))
              (down-right (pos-down-right item)))
            (when (in? down-left)
              (uniq-push! work down-left))
            (when (in? down-right)
              (uniq-push! work down-right))
            (step1 in? work splitters (+ acc 1))))
        (begin
          (let
            ((down (pos-down item)))
            (when (in? down)
              (uniq-push! work down))
            (step1 in? work splitters acc)))))))

;; compute the score for part 1
(define (score1 in? positions)
  (let*
    ((work (make-uniq))
      (start (car (car positions)))
      (splitters (make-hash-table))
      (splitters-positions (apply append (cdr positions))))
    (for-each (lambda (x) (hash-set! splitters x #t)) splitters-positions)
    (uniq-push! work (car (car positions)))
    (step1 in? work splitters 0)))

(define (score2 positions rows cols)
  (let* ((init-vector (make-vector cols 0))
         (splitters (make-hash-table))
         (start-pos (car (car positions)))
         (start-x (match-let ((($ <pos> x y) start-pos)) x))
         (start-y (match-let ((($ <pos> x y) start-pos)) y))
         (splitters-positions (apply append (cdr positions))))
    (for-each (lambda (x) (hash-set! splitters x #t)) splitters-positions)
    (vector-set! init-vector start-y 1) ;; starting count
    (let loop-rows ((x 0)
                    (score 0)
                    (curr-vector init-vector)
                    (next-vector (make-vector cols 0)))
      (if (>= x rows)
        (+ score (apply + (vector->list curr-vector))) ;; sum the previously fallen tachyon and all the remaining one
        (let ((row-score 0))
          (vector-fill! next-vector 0)

          ;; function to propagate the tachyon to the next row and
          ;; update the score for any tachyon falling out side of the grid
          (define (propagate! y tachyons)
            (if (and (>= y 0) (< y cols))
              (let ((prev (vector-ref next-vector y)))
                (vector-set! next-vector y (+ prev tachyons)))
              (set! row-score (+ row-score tachyons))))

          (let loop-cols ((y 0))
            (when (< y cols)
              (let ((superpos (vector-ref curr-vector y))) ;; how many tachyon reaches this position
                (when (> superpos 0) ;; if there are no tachyon, we just skip it
                  (let ((pos (make-pos x y)))
                    (if (hash-ref splitters pos)
                      (begin ;; splitter
                        (propagate! (- y 1) superpos)
                        (propagate! (+ y 1) superpos))
                      (begin ;; no splitter
                        (propagate! y superpos))))))
              (loop-cols (+ y 1))))
          (loop-rows
            (+ x 1)
            (+ score row-score)
            next-vector
            curr-vector))))))

(define (solve filepath)
  (let ((result (readfile-with-header
                 filepath
                 (lambda (l idx) (line->pos #\S l idx))
                 (lambda (l idx) (line->pos #\^ l idx)))))
    (match result
      ((data rows cols)
        (define (in? pos)
          (match-let ((($ <pos> x y) pos))
            (and
              (>= x 0)
              (>= y 0)
              (< y cols)
              (< x cols))))
        (format #t "Part 1: ~a~%" (score1 in? data))
        (format #t "Part 2: ~a~%" (score2 data rows cols))))))

(define (main args)
  (match args
    ((scriptname filepath)
      (solve filepath))
    ((scriptname)
      (solve "../inputs/day07.txt"))
    (_ (format #t "Usage: ~a [filepath]~%" (car args)))))
