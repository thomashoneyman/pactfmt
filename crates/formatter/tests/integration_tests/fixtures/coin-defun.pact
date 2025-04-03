(module coin GOVERNANCE
    (defcap GOVERNANCE () true)
    (defconst MINIMUM_PRECISION 12)
    (defconst MINIMUM_ACCOUNT_LENGTH 3)
    (defconst MAXIMUM_ACCOUNT_LENGTH 256)
    (defconst COIN_CHARSET CHARSET_LATIN1)

    (defun enforce-unit:bool (amount:decimal)
      @doc "Enforce minimum precision allowed for coin transactions"

      (enforce
        (= (floor amount MINIMUM_PRECISION)
           amount)
        (format "Amount violates minimum precision: {}" [amount]))
      )

    (defun validate-account (account:string)
      @doc "Enforce that an account name conforms to the coin contract"
      (enforce
        (is-charset COIN_CHARSET account)
        (format
          "Account does not conform to the coin contract charset: {}"
          [account]))

      (let ((account-length (length account)))

        (enforce
          (>= account-length MINIMUM_ACCOUNT_LENGTH)
          (format
            "Account name does not conform to the min length requirement: {}"
            [account]))

        (enforce
          (<= account-length MAXIMUM_ACCOUNT_LENGTH)
          (format
            "Account name does not conform to the max length requirement: {}"
            [account]))
        )
    )

    (defcap GAS () true)

    (defun gas-only ()
      "Predicate for gas-only user guards."
      (require-capability (GAS)))

    (defun gas-guard (guard:guard)
      "Predicate for gas + single key user guards"
      (enforce-one
        "Enforce either the presence of a GAS cap or keyset"
        [ (gas-only)
          (enforce-guard guard)
        ]))

    (defcap DEBIT (sender:string) true)
    (defun debit:string (account:string amount:decimal)
      (require-capability (DEBIT account))
      true
    )

    (defun buy-gas:string (sender:string total:decimal)
      @doc "This function describes the main 'gas buy' operation. At this point \
      \MINER has been chosen from the pool, and will be validated. The SENDER   \
      \of this transaction has specified a gas limit LIMIT (maximum gas) for    \
      \the transaction, and the price is the spot price of gas at that time.    \
      \The gas buy will be executed prior to executing SENDER's code."

      @model [ (property (> total 0.0))
               (property (valid-account sender))
             ]

      (validate-account sender)

      (enforce-unit total)
      (enforce (> total 0.0) "gas supply must be a positive quantity")

      (require-capability (GAS))
      (with-capability (DEBIT sender)
        (debit sender total))
      )

    (defcap CREDIT (receiver:string) true)
    (defschema coin-schema balance:decimal guard:guard)
    (deftable coin-table:{coin-schema})
    (defun credit:string (account:string guard:guard amount:decimal)
      (require-capability (CREDIT account))
      true
    )

    (defun redeem-gas:string (miner:string miner-guard:guard sender:string total:decimal)
      @doc "This function describes the main 'redeem gas' operation. At this    \
      \point, the SENDER's transaction has been executed, and the gas that      \
      \was charged has been calculated. MINER will be credited the gas cost,    \
      \and SENDER will receive the remainder up to the limit"

      @model [ (property (> total 0.0))
               (property (valid-account sender))
               (property (valid-account miner))
             ]

      (validate-account sender)
      (validate-account miner)
      (enforce-unit total)

      (require-capability (GAS))
      (let*
        ((fee (read-decimal "fee"))
         (refund (- total fee)))

        (enforce-unit fee)
        (enforce (>= fee 0.0)
          "fee must be a non-negative quantity")

        (enforce (>= refund 0.0)
          "refund must be a non-negative quantity")

        (emit-event (TRANSFER sender miner fee))

        (with-capability (CREDIT sender)
          (if (> refund 0.0)
            (with-read coin-table sender
              { "balance" := balance }
              (update coin-table sender
                { "balance": (+ balance refund) }))

            "noop"))

        (with-capability (CREDIT miner)
          (if (> fee 0.0)
            (credit miner miner-guard fee)
            "noop"))
        )

      )

    (defun check-reserved:string (account:string)
      (let ((pfx (take 2 account)))
        (if (= ":" (take -1 pfx)) (take 1 pfx) "")))

    (defun enforce-reserved:bool (account:string guard:guard)
      @doc "Enforce reserved account name protocols."
      (if (validate-principal guard account)
        true
        (let ((r (check-reserved account)))
          (if (= r "")
            true
            (if (= r "k")
              (enforce false "Single-key account protocol violation")
              (enforce false
                (format "Reserved protocol guard violation: {}" [r]))
              )))))

    (defun create-account:string (account:string guard:guard)
      @model [ (property (valid-account account)) ]

      (validate-account account)
      (enforce-reserved account guard)

      (insert coin-table account
        { "balance" : 0.0
        , "guard"   : guard
        })
      true
    )

    (defun get-balance:decimal (account:string)
      (with-read coin-table account
        { "balance" := balance }
        balance
        )
      )

    (defun details:object
      ( account:string )
      (with-read coin-table account
        { "balance" := bal
        , "guard" := g }
        { "account" : account
        , "balance" : bal
        , "guard": g })
      )

    (defcap ROTATE (account:string) true)

    (defun rotate:string (account:string new-guard:guard)
      (with-capability (ROTATE account)
        (with-read coin-table account
          { "guard" := old-guard }

          (enforce-guard old-guard)

          (update coin-table account
            { "guard" : new-guard }
            )))
      )

    (defun precision:integer
      ()
      MINIMUM_PRECISION)

    (defcap TRANSFER (sender:string receiver:string amount:decimal) true)

    (defun transfer:string (sender:string receiver:string amount:decimal)
      @model [ (property conserves-mass)
               (property (> amount 0.0))
               (property (valid-account sender))
               (property (valid-account receiver))
               (property (!= sender receiver)) ]

      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")

      (validate-account sender)
      (validate-account receiver)

      (enforce (> amount 0.0)
        "transfer amount must be positive")

      (enforce-unit amount)

      (with-capability (TRANSFER sender receiver amount)
        (debit sender amount)
        (with-read coin-table receiver
          { "guard" := g }

          (credit receiver g amount))
        )
      )

    (defun transfer-create:string
      ( sender:string
        receiver:string
        receiver-guard:guard
        amount:decimal )

      @model [ (property conserves-mass) ]

      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")

      (validate-account sender)
      (validate-account receiver)

      (enforce (> amount 0.0)
        "transfer amount must be positive")

      (enforce-unit amount)

      (with-capability (TRANSFER sender receiver amount)
        (debit sender amount)
        (credit receiver receiver-guard amount))
      )

    (defcap COINBASE () true)

    (defun coinbase:string (account:string account-guard:guard amount:decimal)
      @doc "Internal function for the initial creation of coins.  This function \
      \cannot be used outside of the coin contract."

      @model [ (property (valid-account account))
               (property (> amount 0.0))
             ]

      (validate-account account)
      (enforce-unit amount)

      (require-capability (COINBASE))
      (emit-event (TRANSFER "" account amount))
      (with-capability (CREDIT account)
        (credit account account-guard amount))
      )

    (defcap REMEDIATE () true)

    (defun remediate:string (account:string amount:decimal)
      @doc "Allows for remediation transactions. This function \
           \is protected by the REMEDIATE capability"
      @model [ (property (valid-account account))
               (property (> amount 0.0))
             ]

      (validate-account account)

      (enforce (> amount 0.0)
        "Remediation amount must be positive")

      (enforce-unit amount)

      (require-capability (REMEDIATE))
      (emit-event (TRANSFER "" account amount))
      (with-read coin-table account
        { "balance" := balance }

        (enforce (<= amount balance) "Insufficient funds")

        (update coin-table account
          { "balance" : (- balance amount) }
          ))
      )

    (defcap GENESIS () true)

    (defun create-allocation-account
      ( account:string
        date:time
        keyset-ref:string
        amount:decimal
      )

      @doc "Add an entry to the coin allocation table. This function \
           \also creates a corresponding empty coin contract account \
           \of the same name and guard. Requires GENESIS capability. "

      @model [ (property (valid-account account)) ]

      (require-capability (GENESIS))

      (validate-account account)
      (enforce (>= amount 0.0)
        "allocation amount must be non-negative")

      (enforce-unit amount)

      (let
        ((guard:guard (keyset-ref-guard keyset-ref)))

        (create-account account guard)
        true
      )
    )

    (defcap RELEASE_ALLOCATION (account:string amount:decimal) true)
    (defschema allocation-schema balance:decimal date:time guard:guard redeemed:bool)
    (deftable allocation-table:{allocation-schema})

    (defun release-allocation
      ( account:string )

      @doc "Release funds associated with allocation ACCOUNT into main ledger.   \
           \ACCOUNT must already exist in main ledger. Allocation is deactivated \
           \after release."
      @model [ (property (valid-account account)) ]

      (validate-account account)

      (with-read allocation-table account
        { "balance" := balance
        , "date" := release-time
        , "redeemed" := redeemed
        , "guard" := guard
        }

        (let ((curr-time:time (at 'block-time (chain-data))))

          (enforce (not redeemed)
            "allocation funds have already been redeemed")

          (enforce
            (>= curr-time release-time)
            (format "funds locked until {}. current time: {}" [release-time curr-time]))

          (with-capability (RELEASE_ALLOCATION account balance)

          (enforce-guard guard)

          (with-capability (CREDIT account)
            (emit-event (TRANSFER "" account balance))
            (credit account guard balance)
            true
          ))
      )))
)