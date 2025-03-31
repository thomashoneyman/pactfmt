(module coin GOVERNANCE
    (defcap GOVERNANCE () true)

    (defconst COIN_CHARSET CHARSET_LATIN1
      "The default coin contract character set")

    (defconst MINIMUM_PRECISION 12
      "Minimum allowed precision for coin transactions")

    (defconst MINIMUM_ACCOUNT_LENGTH 3
      "Minimum account length admissible for coin accounts")

    (defconst MAXIMUM_ACCOUNT_LENGTH 256
      "Maximum account name length admissible for coin accounts")

    (defconst VALID_CHAIN_IDS (map (int-to-str 10) (enumerate 0 19))
      "List of all valid Chainweb chain ids")
)