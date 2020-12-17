open Batteries;;
open Printf;;

let input () = File.lines_of Sys.argv.(1)
             |> Enum.map int_of_string
             |> Array.of_enum
;;

let brute_force a x i j =
  let rec inner y i = if i >= j
                      then false
                      else if a.(i) + y = x
                      then true
                      else inner y (succ i) in
  let rec go i = if i >= j
                 then false
                 else inner (a.(i)) (succ i) || go (succ i) in
  go i
;;

let p1 pa inp =
  let rec find_from i = if not (brute_force inp inp.(i) (i - pa) (i + pa))
                        then inp.(i)
                        else find_from (succ i) in
  find_from pa
;;

let p2 x inp =
  let min_max arr = Array.min arr + Array.max arr in
  let rec go acc i j = if acc = x
                       then min_max (Array.sub inp i (succ j - i))
                       else if acc < x
                       then go (acc + inp.(succ j)) i (succ j)
                       else go (acc - inp.(i)) (succ i) j in
  go (inp.(0) + inp.(1)) 0 1
;;

let () =
  let pa = if String.ends_with Sys.argv.(1) "test.txt" then 5 else 25 in
  let inp = input () in
  inp |> p1 pa |> (fun x -> printf "part1: %d\n" x; p2 x inp) |> printf "part2: %d\n"
;;
