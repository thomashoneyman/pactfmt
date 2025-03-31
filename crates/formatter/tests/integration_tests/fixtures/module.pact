(module my-mod GOV
    (defcap GOV ()
      @event
      true)

    (defcap GAS ()
    "Magic capability to protect gas buy and redeem"
    @managed
    true)

    (defun wot:integer (a:string b:string) (+ 1 b))
    (defun wot2:[[integer]] (a:integer b)
        @doc "This is a docstring"
        @model []
        [1, 2, 3]
        [1 2]))
