(defun mul (lhs rhs)
  (if (= rhs 1)
    lhs
    (+ lhs (mul lhs (+ rhs (negate 1))))))

(defun fact (n)
  (if (= n 1)
    1
    (mul n (fact (+ n (negate 1))))))

(println (list 'ANSWER (fact 5)))
(exit)
