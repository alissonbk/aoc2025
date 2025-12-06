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
  

let in_range v (a, b) = a <= v && v <= b

let puzzle2 =
  let (ranges, _) = read_file in
  let sorted_ranges = List.sort (fun (a1, _) (a2, _) -> a1 - a2) ranges in
  let rec loop ranges prev sum =
    match ranges with
      | [] -> let (prev_a, prev_b) = prev in sum + (prev_b - prev_a + 1)      
      | (a, b) :: t ->
        let (prev_a, prev_b) = prev in
        if in_range a prev then
          loop t ((Int.min prev_a a), (Int.max prev_b b)) sum
        else
          loop t (a, b) (sum + (prev_b - prev_a + 1))
  in  
  loop (List.tl sorted_ranges) (List.hd sorted_ranges) 0






let () =
  printf "puzzle1: %d\n" puzzle1;
  printf "puzzle2: %d\n" puzzle2