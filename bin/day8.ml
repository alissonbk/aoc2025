open Printf


let max_iterations = 1000

type dst = { dst : float; pair: ((int * int * int) * (int * int * int)) }

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

let print_circuits =
  Array.iteri (fun idx circuit ->
    printf "circuit %d: " idx;
    circuit |> List.iter (fun (x, y, z) -> printf "%d, %d, %d   -    " x y z); printf "\n")


let sort_input input =
  let dynarr = Dynarray.create () in  
  for i = 0 to (Array.length input) -1 do     
    for j = 0 to (Array.length input) - 1 do       
      if i < j then (
        let dst = euclidean_dst input.(i) input.(j) in
        Dynarray.add_last dynarr { dst = dst; pair = (input.(i), input.(j))}
      )
    done
  done;
  let arr = Dynarray.to_array dynarr in
  arr |> Array.sort (fun ra rb -> Float.compare ra.dst rb.dst );
  arr

let find_next_empty_arr_pos arr =
  let exception Break of int in  
  try
    for i = 0 to (Array.length arr) - 1 do 
      if List.length arr.(i) = 0 then raise (Break i);
    done;
    (-1)
  with
    | Break i -> i



(* i think the circuit will need to be sorted to work ... *)
let merge_circuits (circuits : 'a list array) ida idb () =  
  (* Array.sort (fun a b -> (List.length a) - (List.length b) ) circuits;         *)
  if ida < idb then (
    circuits.(ida) <- (List.append circuits.(ida) circuits.(idb));
    circuits.(idb) <- []
  ) else (
    circuits.(idb) <- (List.append circuits.(idb) circuits.(ida));
    circuits.(ida) <- []
  )

let calculate_circuits circuits =  
  let mult = ref 1 in  
  for i = 0 to 2 do 
    printf "circuit %d length: %d" i (circuits.(i) |> List.length);
    mult := !mult * (circuits.(i) |> List.length)
  done;
  !mult
  


let find_circuit_member_idx circuits jbox =
  let id = ref (-1) in
  circuits |> Array.iteri (fun idx circuit -> if lst_mem jbox circuit then id := idx);
  !id

let append_circuit circuits jboxa jboxb () =                
    let ida = find_circuit_member_idx circuits jboxa in
    let idb = find_circuit_member_idx circuits jboxb in 
    (* print_circuits circuits; printf "\n\n";    
    print_tupl jboxa;
    print_tupl jboxb;
    printf "circuit member: ida: %d idb : %d\n; circuits length: %d\n" ida idb (Array.length circuits);
    print_circuits circuits; printf "\n\n";     *)
    if ida = (-1) && idb = (-1) then (      
      let set_pos = find_next_empty_arr_pos circuits in
      if set_pos = (-1) then(
        (Array.append circuits (Array.make 1 [jboxa; jboxb]), false))
      else (
        circuits.(set_pos) <- [jboxa; jboxb];
        (circuits, false)
      ) 
    ) else if ida = (-1) then (
      circuits.(idb) <- (jboxa :: circuits.(idb));
      circuits, false
    ) else if idb = (-1) then (
      circuits.(ida) <- (jboxb :: circuits.(ida));
      circuits, false
    ) else if ida <> idb then (       
      printf "mergin circuits %d %d" ida idb;     
      merge_circuits circuits ida idb (); 
      circuits, false      
    ) else if ida = idb then ( circuits, true ) else (failwith "invalid")
  
let () = 
  let input = read_file |> Array.of_list in      
  let return_result circuits () = 
       Array.sort (fun a b -> (List.length b) - (List.length a) ) circuits;        
        calculate_circuits circuits in
  let sorted = sort_input input in
  let rec update_circuits circuits idx arridx () =
    if idx = (max_iterations - 1) then (
      Array.sort (fun a b -> (List.length b) - (List.length a) ) circuits;    
      print_circuits circuits;
      return_result circuits ()
    ) else
    let (ca, cb) = sorted.(arridx).pair in
    (* printf "closest: "; print_tupl ca; print_tupl cb;     *)
    let (new_circuit, both_equal) = append_circuit circuits ca cb () in    
    if both_equal then (       
      update_circuits new_circuit (idx) (arridx + 1) ()
    ) else (
        update_circuits new_circuit (idx + 1) (arridx + 1) ()
    )    
  in  
  let res = update_circuits (Array.make max_iterations []) 0 0 () in
  printf "res: %d\n" res