open Batteries;;

module Set = Set.Char
;;

let input () = File.lines_of Sys.argv.(1)
;;
let process f g init =
  List.fold (fun acc s -> acc + Set.cardinal s) 0
  % (fun (qs, qss) -> g qs :: qss)
  % Enum.fold f (init , [])
;;

let p1 (qs, qss) s = if String.is_empty s
                         then Set.empty, qs :: qss
                         else String.fold_left (fun s c -> Set.add c s) qs s , qss
;;
let p2 (qs, qss) s = if String.is_empty s
                     then None, Option.get qs :: qss
                     else let of_string = Set.of_list % String.explode in
                          match qs with
                          | None -> Some (of_string s), qss
                          | Some qs -> Some (Set.inter (of_string s) qs) , qss
;;

let () =
  input () |> process p1 (fun x -> x) Set.empty |> Printf.printf "part1: %d\n";
  input () |> process p2 Option.get  None |> Printf.printf "part2: %d\n";
;;
