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
let puzzle2 =
    let input = read_file in  
  (* List.iter (fun (hdev, dsts) -> printf "hdev: %s: " hdev; dsts |> List.fold_left (fun acc curr -> acc ^ "-" ^ curr) "" |> printf "%s"; printf "\n") input; *)  
  let rec loop lst curr_dev visted_dac visited_fft cache =
    match lst with
      | [] -> 0
      | (hdev, hdsts) :: t ->              
        if curr_dev = "out" && visted_dac && visited_fft then (          
          1
        ) else
        if hdev = curr_dev then (                              
          let new_dac = visted_dac || curr_dev = "dac" in
          let new_fft = visted_dac || curr_dev = "fft" in                    
          List.fold_left (fun acc curr -> acc + (loop input curr new_dac new_fft (StringSet.add curr_dev cache))) 0 hdsts
        ) else (
          loop t curr_dev visted_dac visited_fft cache
        )
  in
  loop input "svr" false false StringSet.empty

  
  


let () =
  printf "puzzle1 result: %d\n" puzzle1;
  printf "puzzle2 result: %d\n" puzzle2