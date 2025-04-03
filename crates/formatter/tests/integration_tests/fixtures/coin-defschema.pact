(module coin GOVERNANCE
    (defcap GOVERNANCE () true)

    (defschema coin-schema
      @doc "The coin contract token schema"
      @model [ (invariant (>= balance 0.0)) ]

      balance:decimal
      guard:guard)

    (defschema crosschain-schema
      @doc "Schema for yielded value in cross-chain transfers"
      receiver:string
      receiver-guard:guard
      amount:decimal
      source-chain:string)

    (defschema allocation-schema
      @doc "Genesis allocation registry"

      balance:decimal
      date:time
      guard:guard
      redeemed:bool)

    (deftable allocation-table:{allocation-schema})
)