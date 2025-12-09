open Printf


let max_iterations = 10

let read_file =
  let chann = open_in "bin/input8.txt" in
  let rec loop lst =      
    try 
      match input_line chann with
        | s -> s 
          |> String.split_on_char ',' 
          |> List.map int_of_string 
          |> fun e -> match e with | x :: y :: z :: [] -> loop ((x, y, z) :: lst) | _ -> failwith "invalid size"
    with
      | End_of_file -> lst
  in loop []



let euclidean_dst (xa, ya, za) (xb, yb, zb) =
  let dx = float_of_int (xb - xa) in
  let dy = float_of_int (yb - ya) in
  let dz = float_of_int (zb - za) in
  sqrt (dx *. dx +. dy *. dy +. dz *. dz)

let compare_jbox jboxa jboxb = match (jboxa, jboxb) with | (x1, y1, z1), (x2, y2, z2) -> x1 = x2 && y1 = y2 && z1 = z2

let rec lst_mem el = function
    | [] -> false
    | h :: t -> if compare_jbox h el then true else lst_mem el t

let is_pair_mem_any_circuit jboxa jboxb circuits =
  let a_mem = ref false in
  let b_mem = ref false in
  circuits |> Array.iter (fun lst -> if lst_mem jboxa lst then a_mem := true; if lst_mem jboxb lst then b_mem := true);
  !a_mem && !b_mem


let rec find_closest_single jbox closest =  
  function
    | [] -> closest
    | jbox2 :: t ->
      if euclidean_dst jbox jbox2 < euclidean_dst jbox closest then
        find_closest_single jbox jbox2 t
      else
        find_closest_single jbox closest t

let rec find_closest_all input (c1, c2) circuits =
  match input with
    | [] -> (c1, c2)
    | h :: t ->
      let new_closest = (h, find_closest_single h (Int.max_int, Int.max_int, Int.max_int) t) in      
      match new_closest with
        | (ca, cb) -> 
            if is_pair_mem_any_circuit ca cb circuits || euclidean_dst c1 c2 < euclidean_dst ca cb then 
              find_closest_all t (c1, c2) circuits 
            else            
              find_closest_all t (ca, cb) circuits            


let find_next_empty_arr_pos arr =
  let exception Break of int in  
  try
    for i = 0 to (Array.length arr) - 1 do 
      if List.length arr.(i) = 0 then raise (Break i);
    done;
    (-1)
  with
    | Break i -> i
  



let append_circuit circuits jboxa jboxb =    
  let exception Break in
  try
    circuits |> Array.iteri (fun idx circuit ->     
        let is_a_mem = lst_mem jboxa circuit in
        let is_b_mem = lst_mem jboxb circuit in
        (* maybe will cause infinite loop *)
        if is_a_mem && is_b_mem then raise Break;
        if is_a_mem && not is_b_mem then (
          Array.set circuits idx (jboxb :: circuits.(idx));
          raise Break
        );
        if is_b_mem && not is_a_mem then (
          Array.set circuits idx (jboxa :: circuits.(idx));
          raise Break
        )        
      );
      let set_pos = find_next_empty_arr_pos circuits in
      if set_pos = (-1) then
        Array.append circuits (Array.make 1 [jboxa; jboxb])
      else (
        Array.set circuits set_pos [jboxa; jboxb];
        circuits
      )      
  with 
    | Break -> circuits
  
let print_tupl = function | (x, y, z) -> printf "(%d, %d, %d)" x y z
    
let max_int_jbox = (Int.max_int, Int.max_int, Int.max_int)

let puzzle1 =   
  let input = read_file in  
  let exception Break of (int * int * int) list array in
  let rec loop idx circuits =
    if idx = max_iterations then raise (Break circuits) else        
    (* if lst_mem jbox already_added then loop (idx + 1) t circuits already_added else *)
    let closest_pair = find_closest_all input (max_int_jbox, max_int_jbox) circuits in
    match closest_pair with
      | (ca, cb) -> 
        printf "jboxa: "; print_tupl ca; printf "\n";
        printf "jboxb: "; print_tupl cb; printf "\n";
        let updated_circuits = append_circuit circuits ca cb in
        updated_circuits |> Array.iteri (fun idx circuit -> 
        printf "circuit %d: " idx; 
        circuit |> List.iter (fun (x, y, z) -> printf "%d, %d, %d   -    " x y z); printf "\n");
        loop (idx + 1) updated_circuits;
        ()
  in
  try
    loop 0 (Array.make max_iterations []);
    (-1)    
  with
    | Break c ->       
      Array.length c

let () = printf "puzzle1: %d\n" puzzle1