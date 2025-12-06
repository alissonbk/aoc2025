open Printf


let half_string str =
  let len = String.length str in
  (String.sub str 0 (len / 2), String.sub str (len / 2) (len / 2))

let is_invalid_id str () =
  if (int_of_string str) < 10 then false else
  let rec loop group_size =    
    let group_amount = (String.length str) / group_size in
    let first_part = String.sub str 0 group_size in
    let rec l last_part curr_pos idx = 
      if idx = (group_amount - 1) then         
        last_part = first_part && group_size * group_amount = String.length str
      else
      let next_pos = (curr_pos + group_size) in
      let part = String.sub str curr_pos group_size in
      if part <> last_part then false else l part next_pos (idx + 1)
    in
    if (l first_part group_size 0) then 
      true 
    else if group_size + 1 > (String.length str) / 2 || group_size + 1 = (String.length str)  then 
      false 
    else 
      loop (group_size + 1)
  in
  loop 1

let sum_invalid_ids_1 range =
  let (start, endd) = range in  
  let rec loop pos sum =    
    if pos > endd then sum else (      
      let pos_str = string_of_int pos in
      if (String.length pos_str) mod 2 <> 0 then loop (pos + 1) sum else
      let (left, right) = half_string pos_str in      
      if left = right then (
        loop (pos + 1) (sum + pos) 
      )
      else loop (pos + 1) sum
    )
  in
  loop start 0
  
let sum_invalid_ids_2 range =
  let (start, endd) = range in  
  let rec loop pos sum =    
    if pos > endd then sum else (      
      let pos_str = string_of_int pos in      
      if is_invalid_id pos_str () then (        
        loop (pos + 1) (sum + pos) 
      )
      else loop (pos + 1) sum
    )
  in
  loop start 0


let rec remove_leading_zeroes str : string =
  if (String.starts_with ~prefix:"0" str) then
    remove_leading_zeroes @@ String.sub str 1 @@ String.length str - 1
  else
    str

let range_from_str str : (int * int) =
  match String.split_on_char '-' str with
    | l :: r :: [] -> 
      let transform v = v |> remove_leading_zeroes |> int_of_string in
      (transform l, transform r)
    | _ -> failwith "should have a pair in range"


let puzzle sum_fn =
  let chann = open_in "bin/input2.txt" in  
  let rec loop ranges sum =
    match ranges with
      | [] -> sum
      | h :: t -> (                
        loop t @@ sum + (h |> range_from_str |> sum_fn)
      )
  in
  loop (input_line chann |> String.split_on_char ',') 0


let () = 
  printf "result 1: %d\n" (puzzle sum_invalid_ids_1);
  printf "result 2: %d\n" (puzzle sum_invalid_ids_2)




