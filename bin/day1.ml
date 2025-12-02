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
        let (next_v, pass_through_zero) = (match h with
          | R n -> ((curr + n) mod 100, (n / 100) + if (curr + (n mod 100)) > 99 then 1 else 0)
          | L n -> ((curr - n + 100 * 100) mod 100, (n / 100)  + if (curr - (n mod 100)) < 0 then 1 else 0)
        ) in                        
        loop t next_v (sum + pass_through_zero)
  in
  loop input 50 0    


let () = printf "puzzle1 result: %d\n" puzzle1