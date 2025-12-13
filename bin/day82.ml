open Printf


let max_iterations = 1000

let print_tupl = function | (x, y, z) -> printf "(%d, %d, %d)" x y z
let euclidean_dst (xa, ya, za) (xb, yb, zb) =
  let dx = float_of_int (xb - xa) in
  let dy = float_of_int (yb - ya) in
  let dz = float_of_int (zb - za) in
  sqrt (dx *. dx +. dy *. dy +. dz *. dz)

type dst = { dst : float; pos_pair: int * int; jbox_pair: (int * int * int) * (int * int * int) }

(* let print_dst = function | r -> 
  let ((xa, ya, za), (xb, yb, zb)) = r.pair in 
  printf " (%d, %d, %d)-(%d, %d, %d) " xa ya za xb yb zb *)

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

let sort_input input =
  let dynarr = Dynarray.create () in  
  for i = 0 to (Array.length input) -1 do     
    for j = 0 to (Array.length input) - 1 do       
      if i > j then (
        let dst = euclidean_dst input.(i) input.(j) in
        Dynarray.add_last dynarr { dst = dst; pos_pair = (i, j); jbox_pair = (input.(i), input.(j)) }
      )
    done
  done;
  let arr = Dynarray.to_array dynarr in
  arr |> Array.sort (fun ra rb -> Float.compare ra.dst rb.dst );
  arr




let puzzle1 () =  
  let input = read_file |> Array.of_list |> sort_input  in  
  let find_array = Array.init (Array.length input) (fun i -> i) in    
  let conns = ref 0 in
  printf "input size: %d" @@ Array.length input;
  let rec find a = (
    if find_array.(a) == a then (
        a
    ) else (
      find_array.(a) <- (find find_array.(a));
      find_array.(a)
    )    
  ) in  
  let join a b = (find_array.(find(a)) <- find(b)) in
  (* for i = 0 to max_iterations - 1 do      *)
  for i = 0 to (Array.length input) - 1 do         
    let record = input.(i) in
    let a, b = record.pos_pair in
    let (xa, _, _), (xb, _, _) = record.jbox_pair in
    if find(a) != find(b) then (
      conns := !conns + 1;      
      if !conns == max_iterations - 1 then (                
        printf "puzzle 2: %d\n" (xa * xb); flush_all ()
      );
      join a b
    )    
  done;  
  let size_lst = Array.make (Array.length input) 0 in
  for i = 0 to (Array.length input) - 1 do
    size_lst.(find(i)) <- size_lst.(find(i)) + 1
  done;
  size_lst |> Array.sort (fun a b -> b - a); 
  printf "size_lst length: %d\n" (Array.length size_lst);
  printf "size_lst 0: %d 1: %d 2: %d\n" size_lst.(0) size_lst.(1) size_lst.(2);
  size_lst.(0) * size_lst.(1) * size_lst.(2)

let () = 
  printf "result1: %d\n" @@ puzzle1 ()