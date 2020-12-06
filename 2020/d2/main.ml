open Batteries;;

type line =
  { pw : string
  ; min : char
  ; max : char
  ; ch  : char
  }

let line_of_string s =
  let open Scanf in let open Char in
  sscanf s "%d-%d %c: %s"
    (fun min max ch pw -> {min=chr(min-1); max=chr(max-1); ch; pw})
;;
let input () = File.lines_of Sys.argv.(1)
               |> Enum.map line_of_string
               |> Array.of_enum
;;
let p1 : line array -> int =
  let open Char in
  let between (mn : char) mx i = mn < i && i <= chr (code mx + 1) in
  let is_valid (r : line) = between r.min r.max (String.count_char r.pw r.ch |> chr) in
  Array.fold (fun acc x -> if is_valid x then acc + 1 else acc) 0
;;
let p2 : line array -> int =
  let is_at s n c = String.get s (Char.code n) == c in
  let is_valid (r: line) = is_at r.pw r.min r.ch <> is_at r.pw r.max r.ch in
  Array.fold (fun acc x -> if is_valid x then acc + 1 else acc) 0
;;
let () =
  BatGc.set { (Gc.get()) with
              BatGc.max_overhead = 1000001
            ; BatGc.allocation_policy = 0
            ; BatGc.major_heap_increment = 50000 * 8
            }
;;

let () = let inp = input () in

  inp |> p1 |> Printf.printf "Part1: %d\n";
  inp |> p2 |> Printf.printf "Part2: %d\n"
;;
