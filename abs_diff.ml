let abs_diff x y = abs(x-y)
let abs_diff2 = (fun x -> (fun y -> abs (x-y)))
let abs_to_4 = abs_diff2 4

let () = Printf.printf "abs1 %d %d = %d\n" 24 16 (abs_diff 24 4);
  Printf.printf "abs2 %d %d = %d\n" 24 16 (abs_diff2 24 4);
  Printf.printf "abs_to_4 %d %d = %d\n" 24 16 (abs_to_4 24)