(module my-mod GOV
  (defcap GOV () true)

  (defun wot:integer
    (a:string
      ; why though
      b:string) @doc "docstring" @model [  ] (+ 1 b))

  (defun wot2:[[integer]] (a b) [1,
    2, 3] [1 2]))
