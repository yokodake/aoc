open Core;;

let nth x n =
  let ( ** ) = Int.( ** ) in
  (x mod 10 ** n) / (10 ** pred n);;

let rec is_sorted ~f = function
  | x::y::ys -> f x y && is_sorted ~f (y::ys)
  | _        -> true;;

let digits x = List.range 1 7
             |> List.map ~f:(nth x)
             |> List.rev;;

let valid1 x = let ds = digits x in
               is_sorted ~f:(<=) ds && not (is_sorted ~f:(<) ds);;

let valid2 x = digits x
               |> List.group ~break:(<>)
               |> List.exists ~f:(fun g -> List.length g = 2) ;;

let () =
  let (s, e) = let args = Sys.get_argv () in
               if Array.length args <> 3 then
                 (130254, 678275)
               else
                 (int_of_string args.(1), int_of_string args.(2))
    and
      print1 xs = List.length xs |> printf "Silver: %d\n"; xs and
      print2 xs = List.length xs |> printf "Gold: %d\n"
  in
  List.range s e
  |> List.filter ~f:valid1 |> print1
  |> List.filter ~f:valid2 |> print2;;
