(module my-mod GOV
    @model
    [ (defproperty conserves-mass
        (= (column-delta coin-table 'balance) 0.0))

        (defproperty valid-account (account:string)
        (and
            (>= (length account) 3)
            (<= (length account) 256)))
    ]

    (defcap GOV () true)
)

