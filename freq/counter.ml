open Core
open List

(* module StringMap = Map.Make(String)

   type t = StringMap

   let empty = StringMap.empty

   let touch s t =
   let count =
    match StringMap.find_opt t s with
    | None -> 0
    | Some x -> x
   in
   StringMap.add t (count+1) s

   let to_list t = 
   StringMap.fold (fun k v acc -> acc@[(k,v)]) t [] *)

type t = int String.Map.t

let empty = String.Map.empty

let to_list t = Map.to_alist t

let touch t s =
  let count =
    match Map.find t s with
    | None -> 0
    | Some x -> x
  in
  Map.update t s ~f:(fun _ -> (count + 1)) 