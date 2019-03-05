open Printf
let rec rem_stutter l =
  match l with
  | [] | [_] -> l
  | a::b::t -> if a = b then rem_stutter (a::t)
    else a:: rem_stutter (b::t)

let () = 
  let l = [1;2;3;4;5;5;5;6;6;3;3] in
  printf "l = [ ";
  List.iter (printf "%d ") l;
  printf "]\n";
  printf "rem_stutter l = [ ";
  List.iter (printf "%d ") (rem_stutter l);
  printf "]\n"