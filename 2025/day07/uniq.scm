(define-module (uniq)
  #:export(make-uniq uniq-push! uniq-pop! uniq-empty?))
(use-modules (srfi srfi-9)
  (ice-9 q))

(define-record-type <uniq>
  (make-uniq-internal queue set)
  uniq?
  (queue uniq-queue)
  (set uniq-set))

(define (make-uniq)
  (make-uniq-internal (make-q) (make-hash-table)))

(define (uniq-push! uq item)
  (let
    ((q (uniq-queue uq))
      (s (uniq-set uq)))
    (unless (hash-ref s item)
      (hash-set! s item #t)
      (enq! q item))))

(define (uniq-pop! uq)
  (let
    ((q (uniq-queue uq))
      (s (uniq-set uq)))
    (if (q-empty? q)
      #f
      (let ((item (q-pop! q)))
        (hash-remove! s item)
        item))))

(define (uniq-empty? uq)
  (let ((q (uniq-queue uq)))
    (q-empty? q)))
