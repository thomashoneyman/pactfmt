(module coin GOVERNANCE
    (defcap GOVERNANCE () true)

    (defun enforce-unit (amount) true)
    (defun buy-gas (sender total) true)
    (defun redeem-gas (miner miner-guard sender total) true)
    (defun validate-account (account) true)
    (defun credit (receiver guard amount) true)

    (defcap CREDIT (receiver) true)

    (defpact fund-tx (sender:string miner:string miner-guard:guard total:decimal)
      @doc "'fund-tx' is a special pact to fund a transaction in two steps"
      @model [ (property (> total 0.0))
               (property (valid-account sender))
               (property (valid-account miner))
             ]

      (step (buy-gas sender total))
      (step (redeem-gas miner miner-guard sender total))
      )

    (defpact transfer-crosschain:string
      ( sender:string
        receiver:string
        receiver-guard:guard
        target-chain:string
        amount:decimal )

      @model [ (property (> amount 0.0))
               (property (valid-account sender))
               (property (valid-account receiver))
             ]

      (step
        (with-capability
          (validate-account sender)
          (validate-account receiver)

          (enforce (!= "" target-chain) "empty target-chain")
          (enforce (!= (at 'chain-id (chain-data)) target-chain)
            "cannot run cross-chain transfers to the same chain")

          (enforce (> amount 0.0)
            "transfer quantity must be positive")

          (enforce-unit amount)

          (let
            ((crosschain-details
              { "receiver" : receiver
              , "receiver-guard" : receiver-guard
              , "amount" : amount
              , "source-chain" : (at 'chain-id (chain-data))
              }))
            (yield crosschain-details target-chain)
            )))

      (step
        (resume
          { "receiver" := receiver
          , "receiver-guard" := receiver-guard
          , "amount" := amount
          , "source-chain" := source-chain
          }

          (with-capability (CREDIT receiver)
            (credit receiver receiver-guard amount))
          ))
      )
)