open Core;;

let fuel m = (m / 3) - 2;;

let fuel_rec m =
  let rec go m acc =
    let f = fuel m
    in if f <= 0 then acc else go f (acc + f)
  in go m 0;;

let lines () = In_channel.read_lines "d1/input.txt";;

let sum = List.fold_right ~f:(+) ~init:0;;

let p1 ls = sum (List.map ~f:(fun s -> int_of_string s |> fuel) ls);;

let p2 ls = sum (List.map ~f:(fun s -> int_of_string s |> fuel_rec) ls);;

let () =
  let ls = lines ()
  in printf "p1 %d\n" (p1 ls);
     printf "p2 %d\n" (p2 ls);;
