let rec read_sum accum =
  let line = read_float_opt () in
  match line with
  | None -> accum
  | Some x -> read_sum (accum +. x)

let () = Printf.printf "Total : %F\n" (read_sum 0.)