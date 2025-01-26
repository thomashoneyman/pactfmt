(defcap
TRANSFER:bool

  (


    sender:string
    receiver:string
    amount:decimal)

  @managed amount TRANSFER-mgr

)
