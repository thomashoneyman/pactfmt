(module my-mod GOV
    (defcap GOV () true)
    (defun wot:integer (a:string b:string) (+ 1 b))
    (defun wot2:[[integer]] (a b) [1, 2, 3] [1 2]))
