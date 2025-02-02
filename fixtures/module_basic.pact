; This is a Pact module
(module
  ; wot
  basic GOV

  (defcap GOV () true)

  (defun add:integer (x:integer y:integer)
    @doc "adds integers"
      (+ x y))
)
