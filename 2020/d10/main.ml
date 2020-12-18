open Batteries;;
open Printf;;

let input () = File.lines_of Sys.argv.(1)
             |> Enum.map int_of_string
             |> Array.of_enum
;;

let p1 inp =
  let f (p, j1, j3) x = if x - p = 3 then
                          x, j1, succ j3
                        else if x - p = 1 then
                          x, succ j1, j3
                        else x, j1, j3
  in
  Array.fold_left f (0, 0, 0) inp
  |> fun (_, j1, j3) -> j1 * succ j3
;;

let combs j1 = let rec go acc j1 = match j1 with
                | 0 -> acc
                | 1 -> acc
                | n -> go (acc + 2) (pred j1) in
                Int.max 1 (go 0 j1);;
let p2 inp =
  let f (p, acc, j1) x = if x - p = 3 then
                            x, acc * combs j1, 0
                          else
                            x, acc, succ j1
  in
  Array.fold_left f (0, 1, 0) inp
  |> fun (_, acc, j1) -> acc * combs j1

;;

let () = let inp = input () |> tap (Array.sort Int.compare) in
  inp |> p1 |> printf "part1: %d\n";
  inp |> p2 |> printf "part2: %d\n";
;;
