open Printf 




let read_file =
  let chann = open_in "bin/input11.txt" in
  let rec loop lst =
    try    
      let s = input_line chann in  
      let v = s |> 
        String.split_on_char ':' |> 
        (function 
          | dev :: dsts :: [] -> (dev, dsts |> String.split_on_char ' ' |> List.filter (fun a -> a <> "") |> List.map (String.trim))
          | _ -> failwith "invalid split size") in
      loop (v :: lst)
    with
      | End_of_file -> lst
  in  
  loop []

let puzzle1 = 
  let input = read_file in  
  (* List.iter (fun (hdev, dsts) -> printf "hdev: %s: " hdev; dsts |> List.fold_left (fun acc curr -> acc ^ "-" ^ curr) "" |> printf "%s"; printf "\n") input; *)
  let sum = ref 0 in
  let rec loop lst curr_dev =
    match lst with
      | [] -> ()
      | (hdev, hdsts) :: t ->              
        if curr_dev = "out" then (          
          sum := !sum + 1
        ) else
        if hdev = curr_dev then (                    
          List.iter (fun curr -> (loop input curr)) hdsts          
        ) else (
          loop t curr_dev
        )
  in
  loop input "you";
  !sum





module StringSet = Set.Make(String)


let create_ht input =
  List.fold_left (fun acc (dev, dsts) ->
    List.fold_left (fun acc' dst ->
      let current = Hashtbl.find_opt acc' dev |> Option.value ~default:[] in
      Hashtbl.replace acc' dev (dst :: current);
      acc'
    ) acc dsts
  ) (Hashtbl.create 100) input


let puzzle2 =
  let input = read_file |> create_ht in
  
  let memo = Hashtbl.create 100 in
  
  let rec count_paths node visited_dac visited_fft =
    if node = "out" then
      if visited_dac && visited_fft then 1 else 0
    else
      let key = (node, visited_dac, visited_fft) in
      match Hashtbl.find_opt memo key with
      | Some(x) -> x
      | None ->
          let neighbors = Hashtbl.find_opt input node |> Option.value ~default:[] in
          let result = List.fold_left (fun acc neighbor ->
            let new_visited_dac = visited_dac || (neighbor = "dac") in
            let new_visited_fft = visited_fft || (neighbor = "fft") in
            acc + count_paths neighbor new_visited_dac new_visited_fft
          ) 0 neighbors in
          Hashtbl.add memo key result;
          result
  in
  count_paths "svr" false false
  
  


let () =
  printf "puzzle1 result: %d\n" puzzle1;
  printf "puzzle2 result: %d\n" puzzle2