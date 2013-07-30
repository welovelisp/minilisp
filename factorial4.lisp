(defun mul (lhs rhs)
  (if (= rhs 1)
    lhs
    (+ lhs (mul lhs (+ rhs (negate 1))))))

(defun fact (n)
  (if (= n 1)
    1
    (mul n (fact (+ n (negate 1))))))

(defun times (n func)
  (if (= n 0)
    ()
    (car (list
           (times (+ n (negate 1)) func)
           (func (+ n (negate 1)))))))

(times 1000 (lambda (n) (println (list 'ANSWER n (fact 5)))))
(exit)
