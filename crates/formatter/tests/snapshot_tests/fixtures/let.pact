(module test GOV
  (defun testfn ()
    (let ((newbal (- managed requested)))
    (enforce (>= newbal 0.0)
    (format "TRANSFER exceeded for balance {}" [managed]))
    newbal)

    (let ((pfx (take 2 account)))
        (if (= ":" (take -1 pfx)) (take 1 pfx) "")
    )


    (let ((is-new (if (= balance -1.0) (enforce-reserved account guard) false)))

   (write coin-table account
     { "balance" : (if is-new amount (+ balance amount))
     , "guard"   : retg
     }))
  )

)