open Printf


type op = L of int | R of int

let read_file =
  let chann = open_in "bin/input1.txt" in
  let rec loop lst =
    try 
      match input_line chann with
        | s -> (
          let str_len = String.length s in          
          let n = int_of_string (String.sub s 1 (str_len - 1)) in
          match String.get s 0 with
            | c when c = 'R' -> loop (R n :: lst)
            | c when c = 'L' -> loop (L n :: lst)              
            | _ -> failwith "invalid char, expected L or R"
        )
    with
      End_of_file -> lst
  in
  loop []


let puzzle1 =
  let input = read_file |> List.rev in  
  let rec loop lst curr sum =
    match lst with
      | [] -> sum
      | h :: t -> 
        let next_v = (match h with
          | R n ->             
            if (curr + n) > 99 then
              (curr + n) - 100
            else
              curr + n
          | L n ->            
            if (curr - n) < 0 then
              100 + (curr - n)
            else
              curr - n
        ) in
        let next_sum = if next_v = 0 then sum + 1 else sum in
        printf "next_v: %d\n" next_v;
        loop t next_v next_sum
  in
  loop input 50 0  
  (* input |> List.iter (fun s -> match s with | R n -> printf "R %d" n | L n -> printf "L %d" n) *)


let () = printf "puzzle1 result: %d\n" puzzle1