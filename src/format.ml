let payment_request (pr : Types.payment_request) = 
  let is_paid = if pr.paid then " PAID" else "" in
  "(" ^  Js.Date.toString pr.date ^ is_paid ^ "): " ^ pr.memo 
