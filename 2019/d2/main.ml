open Core;;
(* open Printf;; *)

let input () = In_channel.create "d2/input.txt"
               |> In_channel.input_line_exn
               |> String.split ~on:','
               |> Array.of_list
               |> Array.map ~f:int_of_string;;

let interp ~f arr pc =
  let l = arr.(pc + 1) and
      r = arr.(pc + 2) and
      o = arr.(pc + 3)
  in arr.(o) <- f arr.(l) arr.(r);;

let rec eval pc arr : int array =
  let next () = eval (pc + 4) arr
  in match arr.(pc) with
     | 1  -> interp ~f:(+) arr pc; next ()
     | 2  -> interp ~f:( * ) arr pc; next ()
     | 99 -> arr
     | _  -> raise (Failure "fuck");;


let p1 arr = arr.(1) <- 12;
             arr.(2) <- 2;
             Array.get (eval 0 arr) 0;;
let tests1 () =
  let print_bool b = print_endline (string_of_bool b)
  in eval 0 [|1;0;0;0;99|]          |> Array.equal (=) [|2;0;0;0;99|]          |> print_bool;
     eval 0 [|2;3;0;3;99|]          |> Array.equal (=) [|2;3;0;6;99|]          |> print_bool;
     eval 0 [|2;4;4;5;99;0|]        |> Array.equal (=) [|2;4;4;5;99;9801|]     |> print_bool;
     eval 0 [|1;1;1;4;99;5;6;0;99|] |> Array.equal (=) [|30;1;1;4;2;5;6;0;99|] |> print_bool;;

let p2 arr =
  let xs : (int * int) list= let open List
           in range 0 100 >>= (fun x -> range 0 100 |> map ~f:(fun y -> (x,y))) and
      eval' (n,v) = let narr = Array.copy arr
                    in narr.(1) <- n; narr.(2) <- v; Array.get (eval 0 narr) 0 = 19690720
  in List.find ~f:eval' xs |> Option.map ~f:(fun (n, v) -> 100 * n + v)

let () = input () |> p1 |> Printf.printf "Part1: %d\n";
         input () |> p2 |> (fun o -> match o with
                                     | None -> print_endline "fuck.."
                                     | Some x -> Printf.printf "Part2: %d\n" x)

  (* tests ();; *)
