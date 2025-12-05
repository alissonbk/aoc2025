open Printf



let parse_range str =
  let lst = str |> String.split_on_char '-' |> List.map int_of_string in
  match lst with
    | a :: b :: [] -> (a, b)
    | _ -> failwith "invalid range size"    

let read_file =
  let chann = open_in "bin/input5.txt" in
  let rec loop ranges ids is_ids =
    try
      match input_line chann with
        | s -> 
          if s = "" then loop ranges ids true else
          if is_ids then 
            loop ranges ((s |> int_of_string) :: ids) true 
          else 
            loop ((parse_range s) :: ranges) ids is_ids
      with
        | End_of_file -> (ranges, ids)
  in
  loop [] [] false




let rec is_in_range ranges id =
  match ranges with
    | [] -> false
    | h :: t -> 
      (match h with
        | (a, b) -> if id >= a && id <= b then true else is_in_range t id        
      )

let puzzle1 =
  let (ranges, ids) = read_file in
  let rec loop_ids ids sum =
    match ids with
      | [] -> sum
      | h :: t -> if is_in_range ranges h then ( loop_ids t (sum + 1)) else loop_ids t sum
        
  in
  loop_ids ids 0


(* module IntSet = Set.Make(Int)

let rec add_range_to_set set curr endd =
  if curr > endd then set else add_range_to_set (IntSet.add curr set) (curr + 1) endd *)


(* missing check double countings *)
let rec find_interpolations ranges curra currb sum =
  match ranges with
    | [] -> sum
    | (ra, rb) :: t ->
      let s = ref 0 in 
      if currb > ra && currb < rb then (
        s := !s + (currb - ra + 1)
      );
      if curra > ra && curra < rb then (
        s := !s + (rb - curra + 1)
      );
      find_interpolations t curra currb (sum + !s)



let puzzle2 =  
  let (ranges, _) = read_file in  
  let rec loop_ranges ranges sum =
    match ranges with
      | [] -> sum
      | (curr_a, curr_b) :: t ->
        let currdiff = (curr_b - curr_a + 1) - (find_interpolations ranges curr_a curr_b 0) in
        loop_ranges t (sum + currdiff)
        
  in
  loop_ranges ranges 0  
  





let () =
  printf "puzzle1: %d\n" puzzle1;
  printf "puzzle2: %d\n" puzzle2