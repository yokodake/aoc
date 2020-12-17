open Batteries;;

exception P of string

type instr = Jmp of int
           | Acc of int
           | Nop of int
;;
let jmp n = Jmp n;;
let acc n = Acc n;;
let nop n = Nop n;;

let instr_of_string s =
  let open Scanf in
  sscanf s "%s %d" (fun s n -> (match s with
                               | "jmp" -> jmp
                               | "acc" -> acc
                               | "nop" -> nop) n)
;;

let input () = File.lines_of Sys.argv.(1)
             |> Enum.map instr_of_string
             |> Array.of_enum
;;

let p1 inp =
  let v = Array.make (Array.length inp) false in
  let rec eval acc i =
    if v.(i) then
      acc
    else
      ( v.(i) <- true
      ; match inp.(i) with
        | Jmp n -> eval acc (i + n)
        | Acc n -> eval (acc + n) (succ i)
        | Nop _ -> eval acc (succ i)
      ) in
  eval 0 0
;;

let p2 inp =
  let open Array in
  let visited = make (length inp) false in
  let rec eval v acc i =
    if i >= length inp then
      Some acc
    else if v.(i) then
      None
    else ( v.(i) <- true
         ; match inp.(i) with
           | Jmp n -> eval v acc (i + n)
           | Acc n -> eval v (acc + n) (succ i)
           | Nop _ -> eval v acc (succ i)
         ) in
  let rec eval' acc i =
    if i >= length inp then
      acc
    else ( visited.(i) <- true
         ; match inp.(i) with
           | Jmp n -> try_eval acc (succ i) (i + n)
           | Nop n -> try_eval acc (i + n) (succ i)
           | Acc n -> eval' (acc + n) (succ i)
         )
  and try_eval acc i j = (
    match eval (copy visited) acc i with
    | Some acc -> acc
    | None -> eval' acc j)
  in
  eval' 0 0
;;



let () =
  input () |> p1 |> Printf.printf "part1: %d\n";
  input () |> p2 |> Printf.printf "part2: %d\n"
;;
