open Printf



let read_file map_fn =
  let chann = open_in "bin/input6.txt" in  
  let rec loop rows ops =
    try
      match input_line chann with
        | s -> 
          if String.starts_with ~prefix:"+" s then
            loop rows (s |> (String.split_on_char ' ') |> List.filter (fun s -> s <> ""))
          else (            
            let row = s |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") |> List.map map_fn in
            loop (row :: rows) ops
          )
    with
      | End_of_file -> (rows, ops)
  in
  loop [] []


let operator str =
  match str with
    | "+" -> ( + )
    | "-" -> ( - )
    | "*" -> ( * )
    | "/" -> ( / )
    | _ -> failwith "invalid operator string"


let rec update_results1 ops nums acc_results new_results =
  match (ops, nums, acc_results) with
    | ([], [], []) -> List.rev new_results
    | (op_h :: op_t, num_h :: num_t, ar_h :: ar_t) ->
      let new_r = (operator op_h) ar_h num_h in      
      update_results1 op_t num_t ar_t (new_r :: new_results)
    | _ -> failwith "lists should have same length!"

  
let puzzle1 =
  let (rows, ops) = read_file int_of_string in
  let rec loop rows ops acc_results =
    match rows with
      | [] -> acc_results |> List.fold_left (fun curr acc -> curr + acc) 0
      | h :: t ->
        loop t ops (update_results1 ops h acc_results [])
  in  
  loop (List.tl rows) ops (List.hd rows)



(* let rec update_results2 ops nums acc_results new_results =
  match (ops, nums, acc_results) with
    | ([], [], []) -> List.rev new_results
    | (op_h :: op_t, num_h :: num_t, ar_h :: ar_t) ->
      let new_r = (operator op_h) ar_h num_h in      
      update_results2 op_t num_t ar_t (new_r :: new_results)
    | _ -> failwith "lists should have same length!" *)
(* let puzzle2 =
  let (rows, ops) = read_file (fun a -> a) in
  let rec loop rows ops acc_results =
    match rows with
      | [] -> acc_results |> List.fold_left (fun curr acc -> curr + acc) 0
      | h :: t ->
        loop t ops (update_results2 ops h acc_results [])
  in  
  loop (List.tl rows) ops (List.hd rows) *)

let () =  
  printf "puzzle1: %d\n" @@ puzzle1;
  (* printf "puzzle2: %d\n" @@ puzzle2 *)