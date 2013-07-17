(defun append (head tail)
  (if head
    (cons (car head)
          (append (cdr head) tail))
    tail))

(defun length (lst)
  (if lst
    (+ 1 (length (cdr lst)))
    0))

(defun nthcdr (n lst)
  (if (= n 0)
    lst
    (nthcdr (+ n (negate 1)) (cdr lst))))

(defun nth (n lst)
  (car (nthcdr n lst)))

(defun last (lst)
  (nth (+ (length lst) (negate 1)) lst))

(defmacro progn (lst)
  (if (= 1 (length lst))
    (car lst)
    (list 'last
          (list 'list
                (car lst)
                (list 'progn (cdr lst))))))

(defun caar (lst)
  (car (car lst)))

(defun cadar (lst)
  (car (cdr (car lst))))

(defun define-all (vars)
  (if vars
    (cons (list 'define
                (caar vars)
                (cadar vars))
          (define-all (cdr vars)))))

(defmacro let (vars exprs)
  (list 'progn
        (append (define-all vars)
                exprs)))

(defun foo (n)
  (let ((x (+ 1 n))
        (y (+ 2 n)))
    ((println 'foo)
     (println (list 'n n))
     (println (list 'x x))
     (println (list 'y y))
     (+ x y))))

(println (foo 5))

(defun bar (n)
  (let ((x (+ 1 n))
        (y (+ 2 n)))
    ((let ((z (+ 3 n)))
       ((println 'bar)
        (println (list 'n n))
        (println (list 'x x))
        (println (list 'y y))
        (println (list 'z z))
        (+ (+ n x) (+ y z)))))))

(println (bar 5))

(exit)

(println '(unable to let-over-lambda because of missing env copying))

(defun plus (n)
  (let ((lhs n))
    ((lambda (rhs) (+ lhs rhs)))))

(define plus-3 (plus 3))
(println (plus-3 5))

(exit)
