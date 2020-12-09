open Batteries;;
open Lexing;;

module Map = BatSplay.Map (String);;

type contents = (int * string) list;;
type bag = string * contents;;

let input () =
  let abort msg = Printf.fprintf stderr "error %s" msg; flush_all (); exit (-1) in
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  match Parse.start Lex.token lexbuf with
  | exception Lex.SyntaxError msg -> abort msg
  | exception e -> abort "unknown"
  | ws -> ws

;;

let p1 inp =
  let rec go xs = match xs with
    | [] -> false
    | (_, "shiny gold") :: xs -> true
    | (_, x) :: xs -> go xs || go (Map.find x inp) in
  Map.fold (fun k bs acc -> if go bs then acc + 1  else acc) inp 0
;;

let p2 inp =
  let tbl = Hashtbl.create 100 in
  let rec count_shiny bs : int =
    List.fold (fun acc (n, b) -> (n * go b) + acc) 1 bs
  and go bag : int = if bag = "shiny gold" then
                 1
               else match Hashtbl.find_option tbl bag with
                    | Some n -> n
                    | None -> let n = count_shiny (Map.find bag inp) in
                              Hashtbl.add tbl bag n; n
  in
  count_shiny (Map.find "shiny gold" inp) - 1
;;

let () = let inp = input () in
         inp |> p1 |> Printf.printf "part1: %d\n";
         inp |> p2 |> Printf.printf "part2: %d\n"
;;
