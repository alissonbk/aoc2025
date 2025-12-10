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

let max_int_jbox = (Int.max_int, Int.max_int, Int.max_int)
let min_int_jbox = (Int.min_int, 0, Int.min_int)

let print_tupl = function | (x, y, z) -> printf "(%d, %d, %d)" x y z

let euclidean_dst (xa, ya, za) (xb, yb, zb) =
  let dx = float_of_int (xb - xa) in
  let dy = float_of_int (yb - ya) in
  let dz = float_of_int (zb - za) in
  sqrt (dx *. dx +. dy *. dy +. dz *. dz)

let compare_jbox jboxa jboxb = match (jboxa, jboxb) with | (x1, y1, z1), (x2, y2, z2) -> x1 = x2 && y1 = y2 && z1 = z2

let rec lst_mem el = function
    | [] -> false
    | h :: t -> if compare_jbox h el then true else lst_mem el t

let rec pair_lst_mem (compa, compb) = function
    | [] -> false
    | (a, b) :: t -> if compare_jbox compa a && compare_jbox compb b || compare_jbox compb a && compare_jbox compa b then true else pair_lst_mem (compa, compb) t

let is_pair_mem_same_circuit jboxa jboxb circuits =
  let exception Break in
  try
    circuits |> Array.iter (fun lst -> if lst_mem jboxa lst && lst_mem jboxb lst then raise Break);
    false
  with
    Break -> true

let ignore_jbox = (-1, -1, -1),(-1,-1,-1)

let is_ignore_jbox (jboxa, jboxb) = let (iga, igb) = ignore_jbox in compare_jbox jboxa iga && compare_jbox jboxb igb

let get_closest input last_closest =  
  let closest = ref (max_int_jbox, min_int_jbox) in
  for i = 0 to (Array.length input) - 1 do 
    let ca = input.(i) in
    for j = 0 to (Array.length input) - 1 do       
      if j = i then () else(
        let cb = input.(j) in
        match !closest with
          | (c1, c2) ->
            (            
              let lca, lcb = last_closest in
              if (euclidean_dst ca cb) < (euclidean_dst c1 c2) && 
                euclidean_dst ca cb >= euclidean_dst lca lcb && 
                (not (compare_jbox lca ca && compare_jbox lcb cb)) && 
                (not (compare_jbox lca cb && compare_jbox lcb ca))  then (
                closest := (ca, cb)
              )
            )
      )
    done    
  done;
  !closest  

let find_next_empty_arr_pos arr =
  let exception Break of int in  
  try
    for i = 0 to (Array.length arr) - 1 do 
      if List.length arr.(i) = 0 then raise (Break i);
    done;
    (-1)
  with
    | Break i -> i


let calculate_circuits circuits =  
  let mult = ref 1 in  
  for i = 0 to 2 do 
    printf "circuit %d length: %d" i (circuits.(i) |> List.length);
    mult := !mult * (circuits.(i) |> List.length)
  done;
  !mult
  



let append_circuit circuits jboxa jboxb () =    
  let exception Break of bool in  
  try
    Array.sort (fun a b -> (List.length b) - (List.length a) ) circuits;
    circuits |> Array.iteri (fun idx circuit ->     
        let is_a_mem = lst_mem jboxa circuit in
        let is_b_mem = lst_mem jboxb circuit in        
        if is_a_mem && is_b_mem then (raise (Break true));
        if is_a_mem && not is_b_mem then (
          Array.set circuits idx (jboxb :: circuits.(idx));
          raise (Break false)
        );
        if is_b_mem && not is_a_mem then (
          Array.set circuits idx (jboxa :: circuits.(idx));
          raise (Break false)
        )        
      );
      let set_pos = find_next_empty_arr_pos circuits in
      if set_pos = (-1) then
        (
        (Array.append circuits (Array.make 1 [jboxa; jboxb]), false))
      else (
        Array.set circuits set_pos [jboxa; jboxb];
        (circuits, false)
      )      
  with 
    | Break both_equal -> (circuits, both_equal)
  
let () = 
  let input = read_file |> Array.of_list in      
  let return_result circuits = 
       Array.sort (fun a b -> (List.length b) - (List.length a) ) circuits;
        (* circuits |> Array.iteri (fun idx circuit ->
        printf "circuit %d: " idx;
        circuit |> List.iter (fun (x, y, z) -> printf "%d, %d, %d   -    " x y z); printf "\n"); *)
        calculate_circuits circuits in
  let rec update_circuits circuits last_closest idx () =
    if idx = (max_iterations - 1) then (return_result circuits) else
    let (ca, cb) = get_closest input last_closest in
    (* printf "closest: "; print_tupl ca; print_tupl cb; *)    
    let (new_circuit, both_equal) = append_circuit circuits ca cb () in
    if both_equal then (       
      update_circuits new_circuit (ca, cb) (idx) ()
    ) else (
        update_circuits new_circuit (ca, cb) (idx + 1) ()
    )
    
  in
  let res = update_circuits (Array.make max_iterations []) ignore_jbox 0 () in
  printf "res: %d\n" res