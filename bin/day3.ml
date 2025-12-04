open Printf


let explode s = List.init (String.length s) (String.get s)

let read_file =
  let chann = open_in "bin/input3.txt" in

  let rec loop (lst: char list list) =
    try
      match input_line chann with
        | s -> loop @@ (s |> explode) :: lst        
    with
      | End_of_file -> lst
  in
  loop []



let find_higher_joltage lst = 
  let arr = Array.of_list lst in
  let bigger = ref 0 in
  for i = 0 to (List.length lst) - 1 do 
    for j = i + 1 to (List.length lst) - 1 do 
      let n = String.make 1 arr.(i) ^ String.make 1 arr.(j) |> int_of_string in
      if n > !bigger then bigger := n else ()
    done
  done;
  !bigger

let puzzle1 =  
  let rec loop lst sum =
    match lst with
      | [] -> sum
      | h :: t ->
        loop t (sum + find_higher_joltage h)
  in
  loop read_file 0
  
  

let () =
  printf "puzzle1: %d\n" puzzle1;
  
  
