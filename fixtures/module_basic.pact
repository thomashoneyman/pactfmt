; This is a Pact module
(module
  ; wot
  basic GOV

  (defcap GOV () true)

  (defconst ADMIN:string
    @doc "name of the admin"
    "admin" )

  (defun add:integer       (
    ; the first number
    x:integer
    ; the second number
    y:integer)
    @doc "adds integers"
      (+ x y))
)
