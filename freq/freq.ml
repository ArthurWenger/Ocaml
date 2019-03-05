open List
open Printf

let first_n l n = 
  let rec aux l n acc =
    match n, l with
    | _,[] | 0,_ -> acc
    | n, h::t -> aux t (n-1) (acc @ [h])
  in
  aux l n []

let maybe_read_line () =
  try Some(input_line stdin)
  with End_of_file -> None

let get_lines =
  let rec loop acc =
    match maybe_read_line () with
    | Some(line) -> loop (line :: acc)
    | None -> acc
  in
  loop []

let build_counts () =
  let lines = get_lines
  in
  fold_left (Counter.touch) Counter.empty lines

let () = 
  build_counts ()
  |> Counter.to_list
  |> sort (fun (_,x) (_,y) -> compare y x)
  |> (fun l -> first_n l 10)
  |> iter (fun (line, count) -> printf "%3d: %s\n" count line)