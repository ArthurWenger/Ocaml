open Printf

let rec sum a b = 
  if a = 0 then b else 1+ sum (a-1) b

let rec mult a b = 
  if a = 0 then 0 else b + mult (a-1) b

let fibo n =
  let rec fib n1 n2 cpt =
    if cpt <= 0 then n1
    else fib n2 (n1 + n2) (cpt-1)
  in
  fib 0 1 n

let rec fibo2 n = 
  if n <= 1 then n else (fibo2 (n-1) + fibo2 (n-2))

let rec nb_ch n = 
  if n < 10 then 1 else 1 + nb_ch (n/10)

let nb_ch2 n = 1 + n mod 10

let nb_div n = 
  let rec ndiv n d =
    if d > n then 0
    else if n mod d != 0 then ndiv n (d+1)
    else 1 + ndiv n (d+1)
  in 
  ndiv n 1

let is_prime n = nb_div n = 2

let list_fact n = 
  let rec fact n d =
    let d = if d >= n then n-1 else d 
    in
    if is_prime n then [n]
    else if d <= 1 || n <= 1 then []
    else if n mod d != 0 then fact n (d-1)
    else if is_prime d then d :: fact (n/d) d
    else (fact d (d-1)) @ fact (n/d) d
  in
  fact n (n-1)

let list_fact2 n =
  let rec fp n d acc =
    if is_prime n then acc @ [n]
    else if n mod d = 0 then fp (n/d) d (acc @ [d])
    else fp n (d+1) acc  
  in
  fp n 2 []

let rec rev = function
  | [] -> []
  | h::t -> (rev t) @ [h] 

let rev2 l =
  let rec f acc = function
    | [] -> acc
    | h::t -> f (h::acc) t
  in
  f [] l

let max_list l =
  match l with
  | [] -> None
  | h::t ->   let rec maxl res = function
      | [] -> Some res
      | h::t -> if res < h then maxl h t
        else maxl res t
    in
    maxl h l

let rec del_elm_list e = function
  | [] -> []
  | h::t -> if h = e then del_elm_list e t
    else h:: del_elm_list e t

let rec access_list_indice i = function
  | [] -> None
  | h::t -> if i =1 then Some h else access_list_indice (i-1) t

let rec fun4all f = function
  | [] -> []
  | h::t -> (f h) :: fun4all f t

let fun4all2 f l = 
  let rec aux f rem = function
    | [] -> rem
    | h::t -> aux f (rem@[f h]) t
  in
  aux f [] l

let sommelist l1 l2 = 
  let l1 = rev l1 in
  let l2 = rev l2 in
  let rec aux l1 l2 =
    match l1, l2 with
    | [], _ -> rev l2
    | _, [] -> rev l1
    | h1::t1, h2::t2 ->  (aux t1 t2) @ [h1+h2]
  in
  aux l1 l2

let listofstring s =
  let length = String.length s in
  let rec aux s i acc =
    if i >= length then acc
    else aux s (i+1) (acc @ [s.[i]]) 
  in
  aux s 0 []


let rec cptlettre c = function
  | [] -> 0
  | h::t -> let count = if h = c then 1 else 0 in
    count + cptlettre c t 

let rec lettreinlist c = function
  | [] -> false
  | h::t -> h = c || lettreinlist c t


let comptelettres l =
  (* let l = tri... *)
  let rec aux acc acc2 = function
    | [] -> acc
    | h::t -> if lettreinlist h acc2 then aux acc acc2 t
      else let count = 1 + cptlettre h t in
        aux (acc@[h,count]) (acc2@[h]) t
  in
  aux [] [] l

let printcouples l = 
  let rec aux l =
    match l with
    | [] -> ""
    | (c,d)::t -> "(" ^ String.make 1 c ^ ", " ^ string_of_int d ^ ") " ^ aux t
  in
  printf "[ %s ]\n" (aux l)

let print_list ?(msg="") l = 
  if msg = "" then printf "[ "
  else printf "%s = [ " msg;
  List.iter (printf "%d ") l;
  printf "]\n"

let print_list_char ?(msg="") l = 
  if msg = "" then printf "[ "
  else printf "%s = [ " msg;
  List.iter (printf "%c ") l;
  printf "]\n"

let print_option = function
  | None -> "None"
  | Some x -> "Some " ^ string_of_int x

let maxwidth header rows =
  let lengths l = List.map String.length l
  in
  List.fold_left (fun acc row -> List.map2 max (lengths row) acc ) (lengths header) rows


let () = printf "sum 4 5 = %d\n" (sum 4 5);
  printf "mult 4 5 = %d\n" (mult 4 5);
  printf "fib 5 = %d\n" (fibo 5);
  printf "fibo2 5 = %d\n" (fibo2 5);
  printf "nb_ch 50 234 = %d\n" (nb_ch 50234);
  printf "nb_ch2 50 234 = %d\n" (nb_ch2 50234);
  printf "nb_div 6 = %d\n" (nb_div 6);
  printf "is_prime 6 = %b\n" (is_prime 6);
  print_list ~msg:"list_fact 636" (list_fact 636);
  print_list ~msg:"list_fact2 1024" (list_fact2 636);
  let l = [1;2;3;4;5] in
  print_list ~msg:"l" l;
  print_list ~msg:"rev l" (rev l);
  print_list ~msg:"rev2 l" (rev2 l);
  printf "max l = %s\n" (print_option (max_list l));
  print_list ~msg:"del_elem_list l" (del_elm_list 2 l);
  printf "access_list_indice l 5 = %s\n" (print_option (access_list_indice 0 l));
  print_list ~msg:"fun4all f l" (fun4all (fun x -> 2*x) l);
  print_list ~msg:"fun4all2 f l" (fun4all2 (fun x -> 2*x) l);
  let l2 = [1;2] in
  print_list ~msg:"sommelist l1 l2" (sommelist l l2);
  let s = "abracadabra" in
  let ls = listofstring s in
  print_list_char ~msg:"ls" ls;
  let rs = comptelettres ls in
  printcouples rs;
  let foldl = List.fold_left (fun acc e -> acc + e) 0 l 
  in
  printf "%d\n" foldl;
  let headers = ["language";"architect";"first release"]
  in 
  let rows = [ ["Lisp" ;"John McCarthy" ;"1958"] ;
               ["C"    ;"Dennis Ritchie";"1969"] ;
               ["ML"   ;"Robin Milner"  ;"1973"] ;
               ["OCaml";"Xavier Leroy"  ;"1996"] ;
             ]
  in
  print_list (maxwidth headers rows)
