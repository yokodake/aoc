open Batteries;;
let ( let+ ) b f = List.map f b
;;

let input () = File.lines_of Sys.argv.(1)
             |> Array.of_enum
;;

let res xs =
  let wget s n = s.[n mod String.length s] in
  let+ f = [ (fun i -> Some (i * 3))
           ; (fun i -> Some i)
           ; (fun i -> Some (i * 5))
           ; (fun i -> Some (i * 7))
           ; (fun i -> if i land 1 == 0 then Some (i / 2) else None)
           ] in
  Array.fold_righti (fun i s z -> match (f i) with
                            | Some i when (wget s i == '#') -> z + 1
                            | _ -> z) xs 0
;;

let () =
  let res = res (input ()) in
  Printf.printf "part1: %d\n" (List.hd res);
  Printf.printf "part2: %d\n" (List.fold ( * ) 1 res)
;;
