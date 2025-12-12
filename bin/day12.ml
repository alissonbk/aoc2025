open Printf



let explode s = List.init (String.length s) (String.get s)


type region = { size : int; presents_fit : int array }


let rec parse_regions lst (regions : region list) =
  match lst with
    | [] -> regions
    | h :: t -> 
      printf "h: %s\n" h;
      let (a, b) = String.split_on_char ':' h |> (function | a :: b :: [] -> (a, b) | _ -> failwith "invalid len") in        
      let sz = a |> String.split_on_char 'x' |> List.fold_left (fun acc curr -> (int_of_string curr) * acc) 1 in
      let presents_fit = b |> String.trim |> String.split_on_char ' ' |> List.map int_of_string in
      let a = { size = sz; presents_fit = presents_fit |> Array.of_list } in
      parse_regions t (a :: regions)

let read_input =
  let chann = open_in "bin/input12.txt" in
  let rec loop parts accpart =
    try
      let s = input_line chann in      
      if s = "" then (        
        loop (accpart :: parts) []
      ) else (
        loop parts (s :: accpart)
      )      
    with
      | End_of_file -> accpart :: parts
  in
  let parts = loop [] [] |> List.rev |> Array.of_list in
  let rec sum_part lst sum =
   ( match lst with
      | [] -> sum
      | h :: t -> sum_part t (sum + (explode h |> List.fold_left (fun acc curr -> if curr = '#' then acc + 1 else acc) 0))) in
  let grids_size = (Array.length parts) - 1 in
  printf "grids_size: %d" grids_size;
  let sum_pos = Array.make (grids_size) 0 in
  for i = 0 to (Array.length parts) - 2 do 
    sum_pos.(i) <- sum_part parts.(i) 0
  done;

  printf "last part: "; parts.((Array.length parts) - 1) |> List.iter (printf "%s");
  let regions = parse_regions parts.((Array.length parts) - 1) [] in
  (sum_pos, regions)



let puzzle1 = 
  let (sum_pos, regions) = read_input in  
  let rec loop regions sum =
    match regions with
      | [] -> sum
      | h :: t ->        
        let total_present_size = ref 0 in
        h.presents_fit |> Array.iteri (fun idx e -> total_present_size := !total_present_size + (e * sum_pos.(idx)));
        if float_of_int !total_present_size < float_of_int h.size then (
          loop t (sum + 1)
        ) else if float_of_int !total_present_size > float_of_int h.size then (
          loop t sum
        ) else (
          printf "this shouldn't happen total grid size: %f total_present_size %f" (float_of_int !total_present_size) (float_of_int h.size);
          loop t (sum)
        )
  in
  loop regions 0  




let () = 
  printf "1: %d\n" puzzle1


