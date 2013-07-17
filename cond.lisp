(defun not (x)
  (if x () t))

(defun and (lhs rhs)
  (if lhs (if rhs t)))

(defun or (lhs rhs)
  (if lhs t (if rhs t)))

(defun lt (lhs rhs)
  (prim-lt lhs rhs))

(defun gt (lhs rhs)
  (lt rhs lhs))

(defun eq (lhs rhs)
  (and (not (lt lhs rhs))
       (not (gt lhs rhs))))

(defun ne (lhs rhs)
  (not (eq lhs rhs)))

(defun le (lhs rhs)
  (or (lt lhs rhs)
      (eq lhs rhs)))

(defun ge (lhs rhs)
  (or (gt lhs rhs)
      (eq lhs rhs)))

(defun length (lst)
  (if lst
    (+ 1 (length (cdr lst)))
    0))

(defun nthcdr (n lst)
  (if (eq n 0)
    lst
    (nthcdr (+ n (negate 1)) (cdr lst))))

(defun nth (n lst)
  (car (nthcdr n lst)))

(defun last (lst)
  (nth (+ (length lst) (negate 1)) lst))

(defmacro progn (lst)
  (if (eq 1 (length lst))
    (car lst)
    (list 'last
          (list 'list
                (car lst)
                (list 'progn (cdr lst))))))

(println 'progn-macro-test-1)
(progn ((println 'hello)
        (println 'world)))

(println 'progn-macro-test-2)
(println (progn ((println 'hello)
                 (println 'world)
                 (+ 1 1))))

(defmacro when (x lst)
  (list 'if x (list 'progn lst)))

(println 'when-macro-test-1)
(when (eq 1 1)
  ((println 'hello)
   (println 'world)))

(println 'when-macro-test-2)
(println (when (eq 1 1)
           ((println 'hello)
            (println 'world)
            (eq 1 1))))

(defun cadr (lst)
  (car (cdr lst)))

(defun cddr (lst)
  (cdr (cdr lst)))

(defmacro cond (lst)
  (list 'if
        (list 'and (car lst) (ge (length lst) 2))
        (list 'progn (cadr lst))
        (list 'if
              (gt (length lst) 2)
              (list 'cond (cddr lst)))))

(println 'cond-macro-test-1)
(cond ((eq 1 0)
       ((println 'case-1)
        (println 'hello)
        (println 'world))
       (and (eq 2 (+ 1 1)) (gt (+ 1 1) 0))
       ((println 'case-2)
        (println 'hello)
        (println 'world))
       (eq 1 1)
       ((println 'case-3)
        (println 'hello)
        (println 'world))
       t
       ((println 'default)
        (println 'hello)
        (println 'world))))

(println 'cond-macro-test-2)
(println (cond ((eq 1 0)
                ((println 'case-1)
                 (println 'hello)
                 (println 'world)
                 1)
                (and (eq 2 (+ 1 1)) (gt (+ 1 1) 0))
                ((println 'case-2)
                 (println 'hello)
                 (println 'world)
                 (+ 1 2))
                (eq 1 1)
                ((println 'case-3)
                 (println 'hello)
                 (println 'world)
                 (+ 1 3))
                t
                ((println 'default)
                 (println 'hello)
                 (println 'world)
                 (+ 1 4)))))

(exit)
