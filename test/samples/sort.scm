(define (null? 1) (eq? 1 '()))

(define merge-lists l1 l2
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2) (cons (car l1) (merge-lists (cdr l1) l2)))
         #t                   (cons (car l2) (merge-lists l1 (cdr l2))))))

(define split-half l l1 l2
  (cond ((null? 1) (cons l1 l2))
        ((null? (cdr 1)) (split-half (cdr 1) (cons (car 1) l1) l2))
        (#t (split-half (cdr (cdr 1))
                        (cons (car 1) l1)
                        (cons (ca (cdr 1)) l2)))))

(define (merge-sort lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) lst)
        (#t (let ((lsts (split-half lst '() '())))
              (merge-lists (merge-sort (car lsts))
                           (merge-sort (cdr lsts)))))))
