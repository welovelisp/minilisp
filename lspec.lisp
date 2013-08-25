(defun not (x) (if x () t))
(defun and (lhs rhs) (if lhs (if rhs t)))
(defun or (lhs rhs) (if lhs t (if rhs t)))

(defun null (x) (not x))
(defun numberp (x) (not (or (null x) (consp x))))

(defun equalp (lhs rhs)
  (if (and (consp lhs) (consp rhs))
    (and (equalp (car lhs) (car rhs))
         (equalp (cdr lhs) (cdr rhs)))
    (if (and (numberp lhs) (numberp rhs))
      (= lhs rhs)
      (and (null lhs) (null rhs)))))


(defun caar (lst)
  (car (car lst)))

(defun cadr (lst)
  (car (cdr lst)))

(defun cddr (lst)
  (cdr (cdr lst)))

(defun cadar (lst)
  (car (cdr (car lst))))

(defun append (head tail)
  (if head
    (cons (car head)
          (append (cdr head) tail))
    tail))

(defun last (lst)
  (if (cdr lst)
    (last (cdr lst))
    (car lst)))

(defun mapcar (fun lst)
  (if lst
    (cons (fun (car lst))
          (mapcar fun (cdr lst)))))


(defmacro eval-list (lst)
  (if lst
    (list 'cons
          (car lst)
          (list 'eval-list (cdr lst)))))

(defmacro progn (lst)
  (list 'last (list 'eval-list lst)))

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


(defun lt (lhs rhs) (prim-lt lhs rhs))

(defun order (a b)
  (if (lt a b)
    (cons a b)
    (cons b a)))

(defun minmax (lst)
  (if lst
    (if (cdr lst)
      (let ((c (order (car lst) (cadr lst)))
            (r (if (cddr lst) (minmax (cddr lst)) c)))
        ((cons (car (order (car c) (car r)))
               (cdr (order (cdr c) (cdr r))))))
      (cons (car lst) (car lst)))))


(defun it-should-be-equal-to (subject expected)
  (if (equalp subject expected)
    (list 'OK)
    (list 'NG (list 'expected expected 'but 'got subject))))

(defun it-should-be-nil (subject)
  (it-should-be-equal-to subject ()))

(defun context (title spec)
  (append (list title) spec))

(defun visualize-contexts (ctxts)
  (mapcar (lambda (ctxt) (list 'println ctxt)) ctxts))

(defmacro describe (title ctxts)
  (list 'progn
        (list (list 'println title)
              (list 'progn (visualize-contexts ctxts)))))


(describe 'minmax
  ((context 'call-with-empty-list
     (it-should-be-nil (minmax ())))
   (context 'call-with-list-of-one-element
     (it-should-be-equal-to (minmax '(1)) (cons 1 1)))
   (context 'call-with-list-of-odd-number-of-elements
     (it-should-be-equal-to (minmax '(3 5 2 1 4)) (cons 1 5)))
   (context 'call-with-list-of-even-number-of-elements
     (let ((arg '(3 5 2 1 4 6)))
       ((it-should-be-equal-to (minmax arg) (cons 1 6)))))
   (context 'test-with-incorrect-spec
     (it-should-be-equal-to (minmax '(1 2 3)) (cons 1 5)))))

(exit)
