open Printf


let explode s = s |> String.to_seq |> List.of_seq

let read_file =
  let chann = open_in "bin/input4.txt" in  
  let mtx = Array.make_matrix 135 135 '.' in
  let rec loop yidx =
    try
      let arr = Array.make 135 '.' in
      match input_line chann with
        | s -> s 
          |> explode 
          |> List.iteri (fun idx c  -> if c = '@' then arr.(idx) <- '@' else ());
          mtx.(yidx) <- arr;
          loop (yidx + 1)
    with
      | End_of_file -> ()
  in
  loop 0;
  mtx



let sum_roll_element mtx py px =    
  try
    if mtx.(py).(px) = '@' then (
      1
    ) else 
      0
  with
    | Invalid_argument _ -> 0

let sum_all_suroudings ?(should_change=false) mtx py px  =
  let sum = sum_roll_element  mtx (py + 1) px +
  sum_roll_element mtx (py + 1) (px + 1) +
  sum_roll_element mtx (py + 1) (px - 1) +
  sum_roll_element mtx (py - 1) px +
  sum_roll_element mtx (py - 1) (px + 1) +
  sum_roll_element mtx (py - 1) (px - 1) +
  sum_roll_element mtx py (px + 1) +
  sum_roll_element mtx py (px - 1) in
  if should_change && sum < 4 then mtx.(py).(px) <- 'x';
  sum
  
  

let puzzle1 = 
  let mtx = read_file in
  let can_be_accessed = ref 0 in
  for y = 0 to (Array.length mtx) - 1 do 
    for x = 0 to (Array.length mtx.(y) - 1) do      
      let c = mtx.(y).(x) in
      if c = '@' && (sum_all_suroudings mtx y x) < 4 then
        can_be_accessed := !can_be_accessed + 1
    done
  done;
  !can_be_accessed


(* let print_mtx mtx =
  mtx |> Array.iter (fun a -> a |> Array.iter (fun c -> printf "%c" c); printf "\n") *)

let rec puzzle2 tot_sum = 
  let mtx = read_file in
  let can_be_accessed = ref 0 in  
  for y = 0 to (Array.length mtx) - 1 do 
    for x = 0 to (Array.length mtx.(y) - 1) do      
      let c = mtx.(y).(x) in      
      if c = '@' && (sum_all_suroudings ~should_change:true mtx y x) < 4 then
        can_be_accessed := !can_be_accessed + 1
    done
  done;  
  if !can_be_accessed = 0 then tot_sum else
  puzzle2 (tot_sum + !can_be_accessed)

let () = 
  printf "puzzle1 %d\n" puzzle1;
  printf "puzzle2 %d\n" (puzzle2 0)