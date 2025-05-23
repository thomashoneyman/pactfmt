(+ 1 2)
(- 3 4)
(* 5 6)
(/ 7 8)

(= 10 10)
(!= 10 20)
(< 10 20)
(> 20 10)
(<= 10 10)
(>= 10 10)

(map (lambda (x) (+ x 1)) [1 2 3])

(if (= 1 1) "yes" "no")

(let ((x 1) (y 2)) (+ x y))
(let* ((x 1) (y (+ x 1))) (+ x y))

(fold (+) 0 [1 2 3 4 5])
(filter (lambda (x) (> x 2)) [1 2 3 4 5])

(format "Name: {}, Age: {}" ["Alice" 30])

(bind { "a": 1 } { "a" := a } a)