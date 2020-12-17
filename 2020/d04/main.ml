open Opal;;

let fst (x, _) = x
;;
let string s =
  let rec loop s i =
    if i >= String.length s
    then return s
    else exactly s.[i] >> loop s (i + 1) in
  loop s 0
;;
let between_int a b s =  match int_of_string_opt s with
  | Some i -> a <= i && i <= b
  | None -> false
;;
let ends_with s cs =
  let rec go i j = if i < 0
                   then true
                   else cs.[i] == s.[j] && go (pred i) (pred j) in
  go (String.length cs - 1) (String.length s - 1)
;;
let str_for_all ?(from = 0) p s =
  let rec go i = if String.length s == i
                 then true
                 else p (s.[i]) && go (succ i) in
  go from
;;
let is_hex_digit c = ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')
;;

let tap p = p >> return ()
;;

let parsers =
  let byrP s = String.length s == 4 && between_int 1920 2002 s in
  let iyrP s = String.length s == 4 && between_int 2010 2020 s in
  let eyrP s = String.length s == 4 && between_int 2020 2030 s in
  let hgtP s = let ss = (String.sub s 0 (String.length s - 2)) in
               if ends_with s "cm"
               then between_int 150 193 ss
               else if ends_with s "in"
               then between_int 59 76 ss
               else false in
  let hclP s = String.length s == 7 && s.[0] == '#' && str_for_all ~from:1 is_hex_digit s in
  let eclP s = List.mem s ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] in
  let pidP s = String.length s == 9 && str_for_all (fun c -> '0' <= c && c <= '9') s in
  [ ("byr", byrP)
  ; ("iyr", iyrP)
  ; ("eyr", eyrP)
  ; ("hgt", hgtP)
  ; ("hcl", hclP)
  ; ("ecl", eclP)
  ; ("pid", pidP)
  ; ("cid", fun _ -> true)
  ]
;;

type passport = (string * string) list;;

let sepP = tap newline <|> eof ();;
let keyP = choice (List.map (string % fst) parsers);;
let valueP = many1 (exactly '#' <|> alpha_num);;
let kvP = keyP        >>= fun k ->
          exactly ':' >>
          valueP      >>= fun v ->
          return (k, implode v);;
let passportP : ('t , passport list) parser = sep_by (many1 (kvP << space)) sepP
;;

let p1 =
  let is_valid p =
    let l = List.length p in
    if l == 7
    then Option.is_none (List.assoc_opt "cid" p)
    else l == 8
  in List.fold_left (fun acc p -> if is_valid p then acc + 1 else acc) 0
;;
let p2 =
  let rec is_valid n xs = match xs with
    | [] -> n == 0
    | (k, v) :: tl -> if k == "cid"
                      then is_valid n tl
                      else (List.assoc k parsers) v && is_valid (pred n) tl

  in List.fold_left (fun acc p -> if is_valid 7 p then acc + 1 else acc) 0;;
;;

let () = let open Printf in
  match parse passportP (LazyStream.of_channel (open_in Sys.argv.(1))) with
  | Some ans -> printf "p1: %d\n" (p1 ans);
                printf "p2: %d\n" (p2 ans)
  | None -> print_endline "fuck!"
;;
