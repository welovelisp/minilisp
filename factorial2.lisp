(define minus-1 (+ 1 2147483647 2147483647))

(defun mul (lhs rhs)
  (if (= rhs 1)
    lhs
    (+ lhs (mul lhs (+ rhs minus-1)))))

(defun sub (lhs rhs)
  (+ lhs (mul minus-1 rhs)))

(defun fact (n)
  (if (= n 1)
    1
    (mul n (fact (+ n minus-1)))))

(println (list 'ANSWER (fact 5)))
(exit)
