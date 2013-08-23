(defun last (lst)
  (if (cdr lst)
    (last (cdr lst))
    (car lst)))

(defmacro eval-list (lst)
  (if lst
    (list 'cons (car lst) (list 'eval-list (cdr lst)))))

(defmacro progn (lst)
  (list 'last (list 'eval-list lst)))

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

(defun append (head tail)
  (if head
    (cons (car head)
          (append (cdr head) tail))
    tail))

(defmacro let (vars exprs)
  (list 'progn
        (append (define-all vars)
                exprs)))

(defun mapcar (fn lst)
  (if lst
    (cons (fn (car lst)) (mapcar fn (cdr lst)))))

(defun select (fn lst)
  (if lst
    (if (fn (car lst))
      (cons (car lst) (select fn (cdr lst)))
      (select fn (cdr lst)))))

(defun not (x)
  (if x () t))

(defun and (lhs rhs)
  (if lhs (if rhs t)))

(defun or (lhs rhs)
  (if lhs t (if rhs t)))

(defun member (n lst)
  (if lst
    (or (= n (car lst))
        (member n (cdr lst)))))

(defun sum (lst)
  (if lst
    (+ (car lst) (sum (cdr lst)))
    0))

(defun kondo1 (lst)
  (mapcar (lambda (n)
            (sum (select (lambda (m) (not (= n m))) seq)))
          seq))

(defun kondo2 (lst)
  (let ((s (sum seq)))
    ((mapcar (lambda (n) (+ s (negate n))) seq))))


(define seq '(1 5 8 2 4))

(println seq)
(println (kondo1 seq))
(println (kondo2 seq))

(exit)
