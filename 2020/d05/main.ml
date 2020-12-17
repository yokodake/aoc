open Batteries;;

let input () = File.lines_of Sys.argv.(1)
;;

let seat_id s = let ns = String.create (String.length s + 2) in
                ns.[0] <- '0';
                ns.[1] <- 'b';
                let rec go i = if i == String.length s
                               then ns
                               else (ns.[i+2] <- (match s.[i] with
                                                 | 'F' -> '0'
                                                 | 'B' -> '1'
                                                 | 'L' -> '0'
                                                 | 'R' -> '1')
                                    ; go (succ i)) in
                int_of_string (Bytes.to_string (go 0))
;;
let find_seat (arr : int array) : int =
  let rec go prev i = if succ prev != Array.get arr i
                      then succ prev
                      else go (succ prev) (succ i) in
  go (pred @@ Array.get arr 0) 0

;;

let p1 =
  Enum.fold (fun acc x -> Int.max acc (seat_id x)) (-1)
;;

let p2 xs = Enum.map (fun x -> seat_id x) xs
            |> Array.of_enum
            |> tap (Array.fast_sort Int.compare)
            |> find_seat
;;

let () =
  input () |> p1 |> Printf.printf "part1: %d\n";
  input () |> p2 |> Printf.printf "part2: %d\n"
;;
