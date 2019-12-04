open Core;;
(* open Printf;; *)

type m_dir = L | R | U | D;;
type move  =
  { dist : int
  ; dir  : m_dir
  };;

type line =
  { init : int * int
  ; term : int * int
  };;

let parse_move s = let open String in
  let c = String.get (rstrip s) 0 and
      n = drop_prefix s 1 |> int_of_string
  in match c with
     | 'L' -> { dist = n; dir = L}
     | 'U' -> { dist = n; dir = U}
     | 'R' -> { dist = n; dir = R}
     | 'D' -> { dist = n; dir = D}
     | _ -> raise (Failure "fuck");;

let next l m =
  { init = l.term
  ; term = match m.dir with
           | L -> Tuple2.map_fst ~f:(fun x -> x - m.dist) l.term
           | R -> Tuple2.map_fst ~f:(fun x -> x + m.dist) l.term
           | U -> Tuple2.map_snd ~f:(fun x -> x + m.dist) l.term
           | D -> Tuple2.map_snd ~f:(fun x -> x - m.dist) l.term
  };;

let is_intersect l1 l2 =
  let (x1, y1) = l1.init and
      (x2, y2) = l1.term and
      (x3, y3) = l2.init and
      (x4, y4) = l2.term and

      between x (a, b) = (a <= x && x <= b) || (a >= x && x >= b)
  in (between x3 (x1, x2) && between x4 (x1, x2) && between y1 (y3, y4) && between y2 (y3, y4))
     || (between y3 (y1, y2) && between y4 (y1, y2) && between x1 (x3, x4) && between x2 (x3, x4))

let string_of_move m = (match m.dir with
                        | L -> "L"
                        | R -> "R"
                        | U -> "U"
                        | D -> "D") ^ string_of_int m.dist;;
let string_of_t2 (x,y) = " (" ^ string_of_int x ^ "," ^ string_of_int y ^") "
let string_of_line l = "{init="^string_of_t2 l.init^"; term="^string_of_t2 l.term^"}"

let intersection l1 l2 : (int * int) option =
  let determinant (x1, y1) (x2, y2) = (x1 * y2) - (y1 * x2)
  in let detl1 = determinant l1.init l1.term and
         detl2 = determinant l2.init l2.term and
         xl1 = fst l1.init - fst l1.term and
         xl2 = fst l2.init - fst l2.term and
         yl1 = snd l1.init - snd l1.term and
         yl2 = snd l2.init - snd l2.term
     in let xnom = determinant (detl1, xl1) (detl2, xl2) and
            ynom = determinant (detl1, yl1) (detl2, yl2) and
            denom = determinant (xl1, yl1) (xl2, yl2)
        in if not (is_intersect l1 l2) then
             None
           else match denom with
           | 0 -> None
           | _ -> Some (xnom / denom, ynom / denom)
                  |> Option.bind ~f:(fun n -> if Tuple2.equal ~eq1:(=) ~eq2:(=) n (0,0) then
                                                None
                                              else Some n);;

let mdt (x, y) : int = abs x + abs y

let input () : move list list = (* In_channel.create *) "d3/input.txt"
               |> In_channel.read_lines
               |> List.map
                    ~f:(fun l -> String.split ~on:',' l |> List.map ~f:parse_move)

let mk_lines ms =
  let rec go l xs =
    match xs with
    | [] -> [l]
    | x :: xs -> l :: go (next l x) xs
  in match ms with
     | [] -> raise (Failure "fuck")
     | m :: ms -> go (next {init=(0,0);term=(0,0)} m) ms

let p1 (lines : line list list) : int option =
  match lines with
  | [w1;w2] -> List.bind w1 ~f:(fun l -> List.map ~f:(intersection l) w2 |> List.filter ~f:Option.is_some)
               |> Option.all
               |> Option.map ~f:(fun x -> List.map ~f:mdt x |> List.min_elt ~compare:Int.compare)
               |> Option.join
  | _ -> raise (Failure "fuck")

let () = input ()
         |> List.map ~f:mk_lines
         |> p1
         |> Option.value_map ~default:"None" ~f:string_of_int
         |> Printf.printf "p1: %s\n"
         (*

         |> Option.value_map ~default:"None" ~f:(List.to_string ~f:string_of_t2)
         |> print_endline
         |> List.iter ~f:(fun x -> List.to_string ~f:string_of_line x |> print_endline)
          *)
