open Printf


let read_input =
  let chann = open_in "bin/input9.txt" in
  let rec loop lst =
    try
      match input_line chann with      
        | s -> s 
          |> String.split_on_char ',' 
          |> List.map int_of_string 
          |> fun e -> match e with | l :: r :: [] -> loop ((l, r) :: lst) | _ -> failwith "invalid size"    
    with
      | End_of_file -> lst
  in  
  loop []


(* type a = {
  diff : int;
  a : (int * int);
  b : (int * int)
} *)

(* let empty_a = { diff= -1; a = (0, 0); b = (0, 0) } *)

let puzzle1 =
  let input = read_input |> Array.of_list in  
  let largest = ref (-1) in
  for i = 0 to (Array.length input) -1 do 
    let (al, ar) = input.(i) in
    for j = 0 to (Array.length input) - 1 do 
      if j = i then () else
      let (bl, br) = input.(j) in
      let diff = ((abs (bl - al)) + 1) * ((abs (br - ar)) + 1) in
      if diff > !largest then
        largest := diff
    done
  done;
  !largest

let () =
  printf "puzzle1: %d\n" puzzle1