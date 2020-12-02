open Batteries;;

type line =
  { min : int
  ; max : int
  ; ch  : char
  ; pw : string
  }

let line_of_string s =
  let open Scanf in
  sscanf s "%d-%d %c: %s" (fun min max ch pw -> {min;max;ch;pw})
;;

let input () = File.lines_of "d2/input.txt"
               |> Enum.map line_of_string

let p1 =
  let open String in
  let between mn mx i = mn <= i && i <= mx in
  let is_valid (r : line) = between r.min r.max (count_char r.pw r.ch) in
  Enum.count % Enum.filter is_valid
;;

let p2 =
  let is_at s n c = if n >= String.length s
                    then false
                    else String.get s n == c in
  let is_valid (r: line) =
    let (n, m) = r.min - 1 , r.max - 1 in
    (is_at r.pw n r.ch && not (is_at r.pw m r.ch))
    || (is_at r.pw m r.ch && not (is_at r.pw n r.ch)) in
  Enum.count % Enum.filter is_valid
;;

let () =
  input () |> p1 |> Printf.printf "Part1: %d\n";
  input () |> p2 |> Printf.printf "Part2: %d\n"
;;
