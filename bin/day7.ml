open Printf



let input_length = 142

let explode_string s = List.init (String.length s) (String.get s)

let read_input =
  let chann = open_in "bin/input7.txt" in
  let fl = input_line chann in
  let mtx = Array.make input_length (Array.make (String.length fl) '.') in
  mtx.(0) <- fl |> explode_string |> Array.of_list;
  for i = 1 to input_length - 1 do 
    mtx.(i) <- (input_line chann) |> explode_string |> Array.of_list;
  done;
  mtx




let print_matrix mtx =
  mtx |> Array.iter (fun a2 -> a2 |> Array.iter (printf "%s"); printf "\n")

let puzzle1 = 
  let mtx = read_input in      
  let sum = ref 0 in
  for i = 1 to (Array.length mtx) -1 do 
    mtx.(i) |> Array.iteri (fun idx c ->      
      (try
        if c <> '^' && (mtx.(i-1).(idx) = '|' || mtx.(i-1).(idx) = 'S') then (          
          mtx.(i).(idx) <- '|'
        );
      with | Invalid_argument _ -> ());
      if c = '^' then (
        try
          if mtx.(i-1).(idx) = '|' then (
            sum := !sum + 1;
            mtx.(i).(idx + 1) <- '|';
            mtx.(i).(idx - 1) <- '|'
          )
        with
          | Invalid_argument _ -> ()
        
      )
    )
  done;    
  !sum


let is_num c = 
  try
    c |> int_of_string |> ignore;
    true
  with
    | Failure _ -> false

let puzzle2 = 
  let mtx = read_input |> Array.map (fun a -> a |> Array.map (fun c -> String.make 1 c)) in        
  for i = 1 to (Array.length mtx) -1 do 
    mtx.(i) |> Array.iteri (fun idx c -> 
      (try
        if c <> "^" && mtx.(i-1).(idx) = "S" then (
          mtx.(i).(idx) <- "1"
        );
        if c <> "^" && is_num mtx.(i-1).(idx) then (          
          mtx.(i).(idx) <- mtx.(i-1).(idx)
        );
      with | Invalid_argument _ -> ());
    );
    mtx.(i) |> Array.iteri (fun idx c ->      
      if c = "^" then (
        try
          if is_num mtx.(i-1).(idx) then (            
            let curr_number = int_of_string mtx.(i-1).(idx) in
            (if is_num mtx.(i).(idx - 1) then (
              let combined = int_of_string mtx.(i).(idx - 1) + curr_number in
              mtx.(i).(idx - 1) <- string_of_int combined;
            ) else if not @@ is_num mtx.(i).(idx - 1) then (
              mtx.(i).(idx - 1) <- string_of_int curr_number
            ));
            (if is_num mtx.(i).(idx + 1) then (
              let combined = int_of_string mtx.(i).(idx + 1) + curr_number in
              mtx.(i).(idx + 1) <- string_of_int combined;
            ) else if not @@ is_num mtx.(i).(idx + 1) then (
              mtx.(i).(idx + 1) <- string_of_int curr_number
            ));
          )
        with
          | Invalid_argument _ -> ()
      )
    )
  done;  
  print_matrix mtx;
  mtx.(input_length - 1) |> Array.fold_left (fun acc curr -> if is_num curr then (int_of_string curr) + acc else acc ) 0

let () =
  printf "result 1: %d\n" puzzle1;
  printf "result 2: %d\n" puzzle2