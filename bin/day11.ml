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


let puzzle2 = 
  let input = read_file in  
  (* List.iter (fun (hdev, dsts) -> printf "hdev: %s: " hdev; dsts |> List.fold_left (fun acc curr -> acc ^ "-" ^ curr) "" |> printf "%s"; printf "\n") input; *)
  let sum = ref 0 in
  let rec loop lst curr_dev visited_dac visited_fft visited =
    if List.mem curr_dev visited then
      ()
    else      
    (match lst with
      | [] -> ()
      | (hdev, hdsts) :: t ->                      
        if curr_dev = "out" then (  
          if visited_dac && visited_fft then sum := !sum + 1
        ) else
        if hdev = curr_dev then (                    
          List.iter (fun curr -> 
            let visited_dac = visited_dac || curr = "dac" in
            let visited_fft = visited_fft || curr = "fft" in
            (loop input curr visited_dac visited_fft (curr_dev :: visited))
          ) hdsts
        ) else (
          loop t curr_dev visited_dac visited_fft visited
        ))
  in
  loop input "svr" false false [];
  !sum


let () =
  printf "puzzle1 result: %d\n" puzzle1;
  printf "puzzle2 result: %d\n" puzzle2